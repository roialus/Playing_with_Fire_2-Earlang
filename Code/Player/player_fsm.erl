%%%-------------------------------------------------------------------
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% Player FSM Process for Playing with Fire 2
%%% Flow: I/O -> Player -> GN -> CN -> GN -> Player -> I/O
%%% @end
%%% Created : 06. Jul 2025
%%%-------------------------------------------------------------------
-module(player_fsm).
-behaviour(gen_statem).

-export([start_link/5, start_link/6, input_command/2, gn_response/2, inflict_damage/1, set_bot_difficulty/2]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).

%% State functions
-export([idle/3, waiting_gn_response/3, immunity/3, dead/3, disconnected/3]).

-define(TICK, 50). % tick time 50 ms
-define(REQUEST_COOLDOWN, 100). % cooldown between requests to GN (MAYBE TO CHANGE)
-define(IMMUNITY_TIME, 2000). % 2 seconds immunity after damage
-define(DISCONNECT_TIMEOUT, 60000). % 60 seconds until process kill

%% Bot-specific constants
-define(MIN_ACTION_DELAY, 300).  % Minimum delay between bot actions (ms)
-define(MAX_ACTION_DELAY, 800).  % Maximum delay between bot actions (ms)
-define(BOMB_PROBABILITY, 0.15). % Base probability of dropping bomb vs moving

-record(player_data, {
    %% ! this record was changed - we only stored data relevant to the operation of this process,
    %% ! irrelevant stats are held in the appropriate mnesia table.
    % Player identification
    player_number,      % 1/2/3/4
    %% //position,          % [X, Y]
    direction, % desired direction movement - none/up/down/left/right
    movement, % false |{true,TimerRef}
         % todo: changed from next_position to movement&direction, verify consistency in the code
    
    % Process info
    % todo: changed the names of some of these (local_gn, pid, target_gn) - verify consistency
    request_cooldown = 0,   % milliseconds until next GN request allowed
    local_gn = default, % which GN (**registered name**) does the player FSM & IO is physically running on
    local_gn_pid = default, % which gn (**PID**) does the player FSM sends all his problems
    target_gn = default, % Which GN(**registered name**) does the player need to communicate with (in whose quarter is he)
    pid = default,   % this process PID
    io_handler_pid,         % I/O Handler PID
    
    % Connection status
    disconnected = 0,     % counter to 60 (seconds), then kill process
    bot = false,         % true/false - is this a bot player
    
    % Bot-specific fields
    bot_difficulty = easy,  % easy, medium, hard
    bot_last_action = none, % Last action taken by bot
    bot_action_count = 0,   % Number of actions taken by bot
    bot_bomb_cooldown = 0,  % Cooldown before next bomb
    
    % Default stats
    life = 3,
    speed = 1,           % movement speed
    bombs = 1,           % max bombs at the same time
    explosion_radius = 1,
    special_abilities = [], % list of power-ups
    
    % Dynamic state
    bombs_placed = 0,    % currently placed bombs (bombs - bombs_placed = available)
    immunity_timer = none, % reference to immunity timer
    last_request_time = 0  % timestamp of last GN request
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the player FSM
-spec start_link(PlayerNumber::integer(), StartPos::[integer()], GN_Pid::pid(), IsBot::boolean(), IO_pid::pid()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(PlayerNumber, StartPos, GN_Pid, IsBot, IO_pid) ->
    start_link(PlayerNumber, StartPos, GN_Pid, IsBot, IO_pid, easy).

%% @doc Spawns the player FSM with bot difficulty
-spec start_link(PlayerNumber::integer(), StartPos::[integer()], GN_Pid::pid(), IsBot::boolean(), IO_pid::pid(), BotDifficulty::atom()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(PlayerNumber, StartPos, GN_Pid, IsBot, IO_pid, BotDifficulty) ->
    ServerName = list_to_atom("player_" ++ integer_to_list(PlayerNumber)),
    gen_statem:start_link({local, ServerName}, ?MODULE, 
        [PlayerNumber, StartPos, GN_Pid, IsBot, IO_pid, BotDifficulty], []).

%% @doc Send input command from I/O handler
input_command(PlayerPid, Command) ->
    gen_statem:cast(PlayerPid, {input_command, Command}).

%% @doc Send response from GN
gn_response(PlayerNum, Response) ->
    gen_statem:cast(list_to_atom("player_" ++ integer_to_list(PlayerNum)), {gn_response, Response}).

%% @doc Inflict damage on player (from explosion)
inflict_damage(PlayerPid) ->
    gen_statem:cast(PlayerPid, inflict_damage).

%% @doc Set bot difficulty during runtime
set_bot_difficulty(PlayerPid, Difficulty) ->
    gen_statem:cast(PlayerPid, {set_bot_difficulty, Difficulty}).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

callback_mode() ->
    state_functions.

init([PlayerNumber, StartPos, GN_Pid, IsBot, IO_pid, BotDifficulty]) ->
    {_, GN_registered_name} = process_info(GN_Pid, registered_name),
    Data = #player_data{
        player_number = PlayerNumber,
        position = StartPos,
        direction = none,
        local_gn = GN_registered_name,
        target_gn = GN_registered_name, % starting at own GN's quarter
        pid = self(),
        local_gn_pid = GN_Pid,
        bot = IsBot,
        bot_difficulty = BotDifficulty,
        io_handler_pid = IO_pid
    },
    
    % Start cooldown timer
    erlang:send_after(?TICK, self(), tick),
    
    % If bot, start bot behavior
    case IsBot of
        true -> schedule_bot_action(Data);
        false -> ok
    end,
    
    {ok, idle, Data}.

%%%===================================================================
%%% State Functions
%%%===================================================================

%% @doc Idle state - ready to receive commands
idle(cast, {input_command, Command}, Data) ->
    handle_input_command(Command, Data);

idle(cast, {gn_response, _Response}, Data) ->
    % Unexpected GN response in idle state - ignore
    {keep_state, Data};

idle(cast, inflict_damage, Data) ->
    case Data#player_data.life > 1 of
        true ->
            % Take damage, enter immunity
            NewLife = Data#player_data.life - 1,
            NewData = Data#player_data{life = NewLife},
            TimerRef = erlang:send_after(?IMMUNITY_TIME, self(), immunity_end),
            ImmuneData = NewData#player_data{immunity_timer = TimerRef},
            
            % Send ack to I/O if available
            send_io_ack({damage_taken, NewLife}, ImmuneData),
            {next_state, immunity, ImmuneData};
        false ->
            % Player dies
            NewData = Data#player_data{life = 0},
            send_io_ack(player_died, NewData),
            {next_state, dead, NewData}
    end;

idle(cast, {set_bot_difficulty, Difficulty}, Data) when Data#player_data.bot ->
    NewData = Data#player_data{bot_difficulty = Difficulty},
    {keep_state, NewData};

idle(cast, {set_bot_difficulty, _Difficulty}, Data) ->
    % Not a bot, ignore
    {keep_state, Data};

idle(info, tick, Data) ->
    handle_tick(Data);

idle(info, bot_action, Data) when Data#player_data.bot ->
    handle_bot_action(Data);

idle(info, disconnect_check, Data) ->
    handle_disconnect_check(Data);

idle(Type, Event, Data) ->
    handle_common_events(Type, Event, Data).

%% @doc Waiting for GN response state
waiting_gn_response(cast, {input_command, _Command}, Data) ->
    % Ignore new inputs while waiting for GN response
    {keep_state, Data};

waiting_gn_response(cast, {gn_response, Response}, Data) ->
    handle_gn_response(Response, Data);

waiting_gn_response(cast, inflict_damage, Data) ->
    % Can still take damage while waiting for GN response
    idle(cast, inflict_damage, Data);

waiting_gn_response(cast, {set_bot_difficulty, Difficulty}, Data) when Data#player_data.bot ->
    NewData = Data#player_data{bot_difficulty = Difficulty},
    {keep_state, NewData};

waiting_gn_response(info, tick, Data) ->
    handle_tick(Data);

waiting_gn_response(Type, Event, Data) ->
    handle_common_events(Type, Event, Data).

%% @doc Immunity state - cannot take damage
immunity(cast, {input_command, Command}, Data) ->
    handle_input_command(Command, Data);

immunity(cast, {gn_response, Response}, Data) ->
    handle_gn_response(Response, Data);

immunity(cast, inflict_damage, Data) ->
    % Immune to damage - ignore damage
    {keep_state, Data};

immunity(cast, {set_bot_difficulty, Difficulty}, Data) when Data#player_data.bot ->
    NewData = Data#player_data{bot_difficulty = Difficulty},
    {keep_state, NewData};

immunity(info, immunity_end, Data) ->
    % Immunity period ended
    NewData = Data#player_data{immunity_timer = none},
    {next_state, idle, NewData};

immunity(info, tick, Data) ->
    handle_tick(Data);

immunity(info, bot_action, Data) when Data#player_data.bot ->
    handle_bot_action(Data);

immunity(Type, Event, Data) ->
    handle_common_events(Type, Event, Data).

%% @doc Dead state - player is dead
dead(cast, {input_command, _Command}, Data) ->
    % Dead players can't do anything
    send_io_ack(player_dead, Data),
    {keep_state, Data};

dead(cast, {gn_response, _Response}, Data) ->
    {keep_state, Data};

dead(cast, inflict_damage, Data) ->
    % Already dead
    {keep_state, Data};

dead(cast, {set_bot_difficulty, Difficulty}, Data) when Data#player_data.bot ->
    NewData = Data#player_data{bot_difficulty = Difficulty},
    {keep_state, NewData};

dead(info, respawn, Data) ->
    % Respawn logic (if implemented)
    NewData = Data#player_data{life = 3},
    {next_state, idle, NewData};

dead(Type, Event, Data) ->
    handle_common_events(Type, Event, Data).

%% @doc Disconnected state - player disconnected
disconnected(info, disconnect_timeout, Data) ->
    % 60 seconds passed, kill process
    {stop, disconnect_timeout, Data};

disconnected(cast, {input_command, _Command}, Data) ->
    % Player reconnected
    NewData = Data#player_data{disconnected = 0},
    {next_state, idle, NewData};

disconnected(Type, Event, Data) ->
    handle_common_events(Type, Event, Data).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

handle_input_command(Command, Data) ->
    case can_send_request(Data) of
        true ->
            case process_command(Command, Data) of
                {ok, Request, NewData} ->
                    % Send request to local GN
                    gen_server:cast(Data#player_data.local_gn_pid, 
                        {player_message, Request}), % ? removed self() from the tuple, relevant data already in Request
                    
                    % Update cooldown
                    CooldownData = NewData#player_data{
                        request_cooldown = ?REQUEST_COOLDOWN, % ? 
                        last_request_time = erlang:system_time(millisecond)
                    },
                    {next_state, waiting_gn_response, CooldownData};
                {error, Reason} ->
                    % Invalid command
                    send_io_ack({error, Reason}, Data),
                    {keep_state, Data}
            end;
        false ->
            % Still in cooldown
            send_io_ack({error, cooldown}, Data),
            {keep_state, Data}
    end.

process_command(Command, Data) ->
    case Command of
        {move, Direction} ->
            %% * Request = {move_request, "WhoAmI", "TargetGN", "Direction"}
            %% ! CHANGE TO: 
            Request = {move_request, Data#player_data.player_number, Data#player_data.target_gn, Direction},
            %//NextPos = calculate_next_position(Data#player_data.position, Direction),    % calculate next position
            %//Request = {move_request, Data#player_data.position, NextPos, 
            %//          Data#player_data.special_abilities},  % create move request
            NewData = Data#player_data{direction = Direction}, % register request in 'direction'
            {ok, Request, NewData};
            
        drop_bomb ->
            case can_drop_bomb(Data) of
                true ->
                    BombType = get_bomb_type(Data#player_data.special_abilities),   % determine bomb type
                    Request = {drop_bomb_request, Data#player_data.position, 
                              BombType, Data#player_data.explosion_radius}, % create bomb request
                    NewData = Data#player_data{bombs_placed = Data#player_data.bombs_placed + 1},   % increment placed bombs
                    {ok, Request, NewData};
                false ->
                    {error, no_bombs_available} % no bombs available
            end;
            
        ignite_remote ->
            case lists:member(remote_bomb, Data#player_data.special_abilities) of
                true ->
                    Request = {ignite_remote_request},  % create ignite request
                    {ok, Request, Data};    
                false ->
                    {error, no_remote_ability}  % no remote bomb ability
            end;
            
        _ ->
            {error, unknown_command}    
    end.

handle_gn_response(Response, Data) ->
    case Response of
        {move_result, accepted} -> % move successful
            %% todo: don't allow any movement inputs besides 'going back' for the time it takes to be at
            %% todo: half-way point of the movement
            placeholder;
        {move_result, denied} -> % move denied
            %% todo: don't allow any movement request for a short time
            placeholder;

        %% !! this entire section below is outdated
        {move_result, success, NewPos, PickedUpPowerup} ->
            % Move successful
            NewData = update_position(NewPos, Data),    % update player position
            FinalData = apply_powerup(PickedUpPowerup, NewData),    % apply power-up if any
            send_io_ack({move_success, NewPos, PickedUpPowerup}, FinalData),   % send ack to I/O 
            {next_state, idle, FinalData};  % return to idle state
            
        {move_result, failed, Reason} ->
            % Move failed
            send_io_ack({move_failed, Reason}, Data),
            {next_state, idle, Data};   % return to idle state
            
        {bomb_result, success} ->
            % Bomb dropped successfully
            send_io_ack(bomb_dropped, Data),
            {next_state, idle, Data};   % return to idle state
            
        {bomb_result, failed, Reason} ->
            % Bomb drop failed - restore bomb count
            NewData = Data#player_data{bombs_placed = Data#player_data.bombs_placed - 1},
            send_io_ack({bomb_failed, Reason}, NewData),
            {next_state, idle, NewData};    % return to idle state
            
        {bomb_exploded} ->
            % One of player's bombs exploded - restore bomb count
            NewData = Data#player_data{bombs_placed = Data#player_data.bombs_placed - 1},
            {keep_state, NewData};  % no ack needed for explosion
            
        {ignite_result, Count} ->
            % Remote bombs ignited
            send_io_ack({ignited_bombs, Count}, Data),
            {next_state, idle, Data};   % return to idle state
            
        _ ->
            % Unknown response
            send_io_ack({error, unknown_response}, Data),
            {next_state, idle, Data}    % return to idle state
    end.

handle_tick(Data) ->
    % Reduce cooldown
    NewCooldown = max(0, Data#player_data.request_cooldown - ?TICK),
    % Reduce bot bomb cooldown
    NewBombCooldown = max(0, Data#player_data.bot_bomb_cooldown - ?TICK),
    NewData = Data#player_data{
        request_cooldown = NewCooldown,
        bot_bomb_cooldown = NewBombCooldown
    }, 
    
    % Schedule next tick
    erlang:send_after(?TICK, self(), tick),
    
    {keep_state, NewData}.

handle_bot_action(Data) when Data#player_data.bot ->
    % Generate bot action based on difficulty
    Action = generate_bot_action(Data),
    
    % Update bot state
    NewData = Data#player_data{
        bot_action_count = Data#player_data.bot_action_count + 1,
        bot_last_action = Action
    },
    
    % Schedule next bot action
    schedule_bot_action(NewData),
    
    % Process bot action as if it came from I/O
    handle_input_command(Action, NewData).

handle_disconnect_check(Data) ->
    case Data#player_data.disconnected of
        Count when Count >= 60 ->
            {stop, disconnect_timeout, Data};   % 60 seconds passed, stop the process
        Count ->
            NewData = Data#player_data{disconnected = Count + 1},
            erlang:send_after(1000, self(), disconnect_check),
            {keep_state, NewData}   % increment disconnect counter
    end.

handle_common_events(_Type, _Event, Data) ->
    {keep_state, Data}. % default handler for unexpected events

%%%===================================================================
%%% Bot AI Functions
%%%===================================================================

%% @doc Generate bot action based on difficulty and current state
generate_bot_action(Data) ->
    case Data#player_data.bot_difficulty of
        easy -> generate_easy_action(Data);
        medium -> generate_medium_action(Data);
        hard -> generate_hard_action(Data)
    end.

%% @doc Easy bot - mostly random movement, occasional bombs
generate_easy_action(Data) ->
    case rand:uniform() < ?BOMB_PROBABILITY andalso Data#player_data.bot_bomb_cooldown =< 0 of
        true ->
            drop_bomb;
        false ->
            Directions = [up, down, left, right],
            Direction = lists:nth(rand:uniform(length(Directions)), Directions),
            {move, Direction}
    end.

%% @doc Medium bot - smarter movement patterns, better bomb timing
generate_medium_action(Data) ->
    case should_drop_bomb_medium(Data) of
        true ->
            drop_bomb;
        false ->
            case should_change_direction(Data) of
                true ->
                    get_smart_direction(Data);
                false ->
                    % Continue last direction or pick new one
                    case Data#player_data.bot_last_action of
                        {move, Dir} -> {move, Dir};
                        _ -> get_smart_direction(Data)
                    end
            end
    end.

%% @doc Hard bot - advanced strategies, optimal bomb placement, evasion
generate_hard_action(Data) ->
    case should_drop_bomb_hard(Data) of
        true ->
            drop_bomb;
        false ->
            case should_use_special_ability(Data) of
                {true, Action} ->
                    Action;
                false ->
                    get_tactical_direction(Data)
            end
    end.

%% @doc Determine if medium bot should drop bomb
should_drop_bomb_medium(Data) ->
    % Drop bomb every 8-12 actions, with some randomness
    ActionMod = Data#player_data.bot_action_count rem 10,
    BombWindow = ActionMod >= 8 andalso ActionMod =< 12,
    RandomFactor = rand:uniform() < 0.3,
    BombWindow andalso RandomFactor andalso Data#player_data.bot_bomb_cooldown =< 0.

%% @doc Determine if hard bot should drop bomb
should_drop_bomb_hard(Data) ->
    % More strategic bomb placement
    ActionMod = Data#player_data.bot_action_count rem 15,
    BombWindow = ActionMod >= 10 andalso ActionMod =< 13,
    RandomFactor = rand:uniform() < 0.4,
    BombWindow andalso RandomFactor andalso Data#player_data.bot_bomb_cooldown =< 0.

%% @doc Check if bot should change direction (medium difficulty)
should_change_direction(Data) ->
    case Data#player_data.bot_last_action of
        {move, _} ->
            % Change direction every 3-5 moves
            MovesInDirection = Data#player_data.bot_action_count rem 4,
            MovesInDirection =:= 0 orelse rand:uniform() < 0.2;
        _ ->
            true
    end.

%% @doc Get smart direction for medium bot
get_smart_direction(_Data) ->
    % For now, just random. Could be enhanced with game state awareness
    Directions = [up, down, left, right],
    Direction = lists:nth(rand:uniform(length(Directions)), Directions),
    {move, Direction}.

%% @doc Check if hard bot should use special abilities
should_use_special_ability(_Data) ->
    % Randomly use remote bomb ignition
    case rand:uniform() < 0.05 of
        true -> {true, ignite_remote};
        false -> false
    end.

%% @doc Get tactical direction for hard bot
get_tactical_direction(Data) ->
    % Advanced movement logic - for now similar to medium
    % Could be enhanced with pathfinding, enemy avoidance, etc.
    get_smart_direction(Data).

%% @doc Schedule next bot action based on difficulty
schedule_bot_action(Data) ->
    Delay = case Data#player_data.bot_difficulty of
        easy -> ?MIN_ACTION_DELAY + rand:uniform(?MAX_ACTION_DELAY - ?MIN_ACTION_DELAY);
        medium -> ?MIN_ACTION_DELAY + rand:uniform((?MAX_ACTION_DELAY - ?MIN_ACTION_DELAY) div 2);
        hard -> ?MIN_ACTION_DELAY + rand:uniform((?MAX_ACTION_DELAY - ?MIN_ACTION_DELAY) div 3)
    end,
    erlang:send_after(Delay, self(), bot_action).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% Helper functions
can_send_request(Data) ->
    Data#player_data.request_cooldown =< 0. % can send request if cooldown is 0 or less

can_drop_bomb(Data) ->
    Data#player_data.bombs_placed < Data#player_data.bombs. % can drop bomb if placed bombs < max bombs

% ! noted to supress compilation errors
%calculate_next_position([X, Y], Direction) ->
%    case Direction of
%        up    -> [X, Y-1];
%        down  -> [X, Y+1];
%        left  -> [X-1, Y];
%        right -> [X+1, Y]
%    end.

update_position(NewPos, Data) ->
    %! change is to supress compilation errors for now
    ok.
    %Data#player_data{position = NewPos, next_position = NewPos}.

apply_powerup(none, Data) ->
    Data;
apply_powerup(Powerup, Data) ->
    case Powerup of
        movespeed ->
            Data#player_data{speed = Data#player_data.speed + 1};
        more_bombs ->
            Data#player_data{bombs = Data#player_data.bombs + 1};
        range ->
            Data#player_data{explosion_radius = Data#player_data.explosion_radius + 1};
        extra_life ->
            Data#player_data{life = Data#player_data.life + 1};
        _ ->
            % Add to special abilities
            NewAbilities = [Powerup | Data#player_data.special_abilities],
            Data#player_data{special_abilities = NewAbilities}      
    end.

%% Determine bomb type based on abilities
get_bomb_type(Abilities) ->
    case lists:member(remote_bomb, Abilities) of
        true -> remote; 
        false ->
            case lists:member(repeating_bomb, Abilities) of
                true -> repeating;
                false -> regular
            end
    end.

send_io_ack(Response, Data) ->
    case Data#player_data.io_handler_pid of
        undefined -> ok;
        Pid -> gen_server:cast(Pid, {player_ack, Response})
    end.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%% Need to add kick, freeze?