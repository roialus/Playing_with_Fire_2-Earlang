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

-export([start_link/4, input_command/2, gn_response/2, inflict_damage/1]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).

%% State functions
-export([idle/3, waiting_gn_response/3, immunity/3, dead/3, disconnected/3]).

-define(TICK, 50). % tick time 50 ms
-define(REQUEST_COOLDOWN, 100). % cooldown between requests to GN (MAYBE TO CHANGE)
-define(IMMUNITY_TIME, 2000). % 2 seconds immunity after damage
-define(DISCONNECT_TIMEOUT, 60000). % 60 seconds until process kill

-record(player_data, {
    % Player identification
    player_number,      % 1/2/3/4
    position,          % [X, Y]
    next_position,     % [X', Y'] - intended next position
    
    % Process info
    request_cooldown = 0,  % milliseconds until next GN request allowed
    original_node_id,      % node where player was created
    process_id,           % this process PID
    gn_pid,              % GN PID
    io_handler_pid,      % I/O Handler PID
    
    % Connection status
    disconnected = 0,     % counter to 60 (seconds), then kill process
    bot = false,         % true/false - is this a bot player
    
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
-spec start_link(PlayerNumber::integer(), StartPos::[integer()], GN_Pid::pid(), IsBot::boolean()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(PlayerNumber, StartPos, GN_Pid, IsBot) ->
    ServerName = list_to_atom("player_" ++ integer_to_list(PlayerNumber)),
    gen_statem:start_link({local, ServerName}, ?MODULE, 
        [PlayerNumber, StartPos, GN_Pid, IsBot, self()], []).

%% @doc Send input command from I/O handler
input_command(PlayerPid, Command) ->
    gen_statem:cast(PlayerPid, {input_command, Command}).

%% @doc Send response from Game Node
gn_response(PlayerPid, Response) ->
    gen_statem:cast(PlayerPid, {gn_response, Response}).

%% @doc Inflict damage on player (from explosion)
inflict_damage(PlayerPid) ->
    gen_statem:cast(PlayerPid, inflict_damage).



%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

callback_mode() ->
    state_functions.

init([PlayerNumber, StartPos, GN_Pid, IsBot, ProcessId]) ->
    Data = #player_data{
        player_number = PlayerNumber,
        position = StartPos,
        next_position = StartPos,
        original_node_id = node(),
        process_id = ProcessId,
        gn_pid = GN_Pid,
        bot = IsBot
    },
    
    % Start cooldown timer
    erlang:send_after(?TICK, self(), tick),
    
    % If bot, start bot behavior
    case IsBot of
        true -> erlang:send_after(500, self(), bot_action);
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
                    % Send request to GN
                    gen_server:cast(Data#player_data.gn_pid, 
                        {player_request, Request, self()}),
                    
                    % Update cooldown
                    CooldownData = NewData#player_data{
                        request_cooldown = ?REQUEST_COOLDOWN,
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
            NextPos = calculate_next_position(Data#player_data.position, Direction),    % calculate next position
            Request = {move_request, Data#player_data.position, NextPos, 
                      Data#player_data.special_abilities},  % create move request
            NewData = Data#player_data{next_position = NextPos},    % update next position
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
    NewData = Data#player_data{request_cooldown = NewCooldown}, 
    
    % Schedule next tick
    erlang:send_after(?TICK, self(), tick),
    
    {keep_state, NewData}.  

handle_bot_action(Data) when Data#player_data.bot ->
    % Simple bot AI - random actions
    Actions = [{move, up}, {move, down}, {move, left}, {move, right}, drop_bomb],
    Action = lists:nth(rand:uniform(length(Actions)), Actions),
    
    % Schedule next bot action
    erlang:send_after(rand:uniform(1000) + 500, self(), bot_action),
    
    % Process bot action as if it came from I/O
    handle_input_command(Action, Data).

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

%% Helper functions
can_send_request(Data) ->
    Data#player_data.request_cooldown =< 0. % can send request if cooldown is 0 or less

can_drop_bomb(Data) ->
    Data#player_data.bombs_placed < Data#player_data.bombs. % can drop bomb if placed bombs < max bombs

calculate_next_position([X, Y], Direction) ->
    case Direction of
        up    -> [X, Y-1];
        down  -> [X, Y+1];
        left  -> [X-1, Y];
        right -> [X+1, Y]
    end.

update_position(NewPos, Data) ->
    Data#player_data{position = NewPos, next_position = NewPos}.

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