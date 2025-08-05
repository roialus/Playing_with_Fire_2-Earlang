-module(bot_handler).

-behaviour(gen_server).

%% API
-export([start_link/2, set_player_pid/2, set_difficulty/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(MIN_ACTION_DELAY, 300).  % Minimum delay between actions (ms)
-define(MAX_ACTION_DELAY, 800).  % Maximum delay between actions (ms)
-define(BOMB_PROBABILITY, 0.15). % Probability of dropping bomb vs moving

-record(bot_state, {
    player_pid = none,              % Player FSM PID
    player_number,                  % Player number (1-4)
    waiting_for_ack = false,        % Waiting for player response
    action_buffer = [],             % Buffered actions while waiting
    difficulty = easy,              % easy, medium, hard
    last_action = none,             % Last action taken
    action_count = 0,               % Number of actions taken
    bomb_cooldown = 0               % Cooldown before next bomb
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start bot handler for a player
start_link(PlayerNumber, Difficulty) ->
    ServerName = list_to_atom("bot_handler_" ++ integer_to_list(PlayerNumber)),
    gen_server:start_link({local, ServerName}, ?MODULE, 
        [PlayerNumber, Difficulty], []).

%% @doc Set the player PID after player FSM starts
set_player_pid(BotHandlerPid, PlayerPid) ->
    gen_server:call(BotHandlerPid, {set_player_pid, PlayerPid}).

%% @doc Change bot difficulty during runtime
set_difficulty(BotHandlerPid, Difficulty) ->
    gen_server:call(BotHandlerPid, {set_difficulty, Difficulty}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([PlayerNumber, Difficulty]) ->
    State = #bot_state{
        player_number = PlayerNumber,
        difficulty = Difficulty
    },
    
    % Start bot action timer
    schedule_next_action(State),
    
    {ok, State}.

handle_call({set_player_pid, PlayerPid}, _From, State) ->
    NewState = State#bot_state{player_pid = PlayerPid},
    {reply, ok, NewState};

handle_call({set_difficulty, Difficulty}, _From, State) ->
    NewState = State#bot_state{difficulty = Difficulty},
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, unknown_request, State}.

handle_cast({player_ack, Response}, State) ->
    % Player acknowledged our command
    NewState = State#bot_state{waiting_for_ack = false},
    
    % Log response for debugging (optional)
    log_bot_response(Response, State#bot_state.player_number),
    
    % Process any buffered actions
    case NewState#bot_state.action_buffer of
        [] -> 
            {noreply, NewState};
        [NextAction | Rest] ->
            UpdatedState = NewState#bot_state{action_buffer = Rest},
            process_bot_action(NextAction, UpdatedState)
    end;

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(bot_action, State) ->
    % Generate and process bot action
    Action = generate_bot_action(State),
    NewState = case process_bot_action(Action, State) of
        {noreply, S} -> S;
        _ -> State
    end,
    
    % Schedule next action
    schedule_next_action(NewState),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

process_bot_action(Action, State) ->
    case State#bot_state.waiting_for_ack of
        true ->
            % Buffer action while waiting for ack
            NewBuffer = State#bot_state.action_buffer ++ [Action],
            NewState = State#bot_state{action_buffer = NewBuffer},
            {noreply, NewState};
        false ->
            % Send command to player FSM
            case State#bot_state.player_pid of
                undefined ->
                    io:format("Bot ~p: No player process connected~n", 
                        [State#bot_state.player_number]),
                    {noreply, State};
                PlayerPid ->
                    player_fsm:input_command(PlayerPid, Action),
                    NewState = State#bot_state{
                        waiting_for_ack = true,
                        last_action = Action,
                        action_count = State#bot_state.action_count + 1
                    },
                    {noreply, NewState}
            end
    end.

%% @doc Generate bot action based on difficulty and current state
generate_bot_action(State) ->
    case State#bot_state.difficulty of
        easy -> generate_easy_action(State);
        medium -> generate_medium_action(State);
        hard -> generate_hard_action(State)
    end.

%% @doc Easy bot - mostly random movement, occasional bombs
generate_easy_action(State) ->
    case rand:uniform() < ?BOMB_PROBABILITY andalso State#bot_state.bomb_cooldown =< 0 of
        true ->
            drop_bomb;
        false ->
            Directions = [up, down, left, right],
            Direction = lists:nth(rand:uniform(length(Directions)), Directions),
            {move, Direction}
    end.

%% @doc Medium bot - smarter movement patterns, better bomb timing
generate_medium_action(State) ->
    case should_drop_bomb_medium(State) of
        true ->
            drop_bomb;
        false ->
            case should_change_direction(State) of
                true ->
                    get_smart_direction(State);
                false ->
                    % Continue last direction or pick new one
                    case State#bot_state.last_action of
                        {move, Dir} -> {move, Dir};
                        _ -> get_smart_direction(State)
                    end
            end
    end.

%% @doc Hard bot - advanced strategies, optimal bomb placement, evasion
generate_hard_action(State) ->
    case should_drop_bomb_hard(State) of
        true ->
            drop_bomb;
        false ->
            case should_use_special_ability(State) of
                {true, Action} ->
                    Action;
                false ->
                    get_tactical_direction(State)
            end
    end.

%% @doc Determine if medium bot should drop bomb
should_drop_bomb_medium(State) ->
    % Drop bomb every 8-12 actions, with some randomness
    ActionMod = State#bot_state.action_count rem 10,
    BombWindow = ActionMod >= 8 andalso ActionMod =< 12,
    RandomFactor = rand:uniform() < 0.3,
    BombWindow andalso RandomFactor andalso State#bot_state.bomb_cooldown =< 0.

%% @doc Determine if hard bot should drop bomb
should_drop_bomb_hard(State) ->
    % More strategic bomb placement
    ActionMod = State#bot_state.action_count rem 15,
    BombWindow = ActionMod >= 10 andalso ActionMod =< 13,
    RandomFactor = rand:uniform() < 0.4,
    BombWindow andalso RandomFactor andalso State#bot_state.bomb_cooldown =< 0.

%% @doc Check if bot should change direction (medium difficulty)
should_change_direction(State) ->
    case State#bot_state.last_action of
        {move, _} ->
            % Change direction every 3-5 moves
            MovesInDirection = State#bot_state.action_count rem 4,
            MovesInDirection =:= 0 orelse rand:uniform() < 0.2;
        _ ->
            true
    end.

%% @doc Get smart direction for medium bot
get_smart_direction(_State) ->
    % For now, just random. Could be enhanced with game state awareness
    Directions = [up, down, left, right],
    Direction = lists:nth(rand:uniform(length(Directions)), Directions),
    {move, Direction}.

%% @doc Check if hard bot should use special abilities
should_use_special_ability(_State) ->
    % Randomly use remote bomb ignition
    case rand:uniform() < 0.05 of
        true -> {true, ignite_remote};
        false -> false
    end.

%% @doc Get tactical direction for hard bot
get_tactical_direction(State) ->
    % Advanced movement logic - for now similar to medium
    % Could be enhanced with pathfinding, enemy avoidance, etc.
    get_smart_direction(State).

%% @doc Schedule next bot action based on difficulty
schedule_next_action(State) ->
    Delay = case State#bot_state.difficulty of
        easy -> ?MIN_ACTION_DELAY + rand:uniform(?MAX_ACTION_DELAY - ?MIN_ACTION_DELAY);
        medium -> ?MIN_ACTION_DELAY + rand:uniform((?MAX_ACTION_DELAY - ?MIN_ACTION_DELAY) div 2);
        hard -> ?MIN_ACTION_DELAY + rand:uniform((?MAX_ACTION_DELAY - ?MIN_ACTION_DELAY) div 3)
    end,
    erlang:send_after(Delay, self(), bot_action).

%% @doc Log bot responses (for debugging)
log_bot_response(Response, PlayerNumber) ->
    case Response of
        {move_success, _NewPos, _Powerup} ->
            ok; % Don't log successful moves to reduce noise
        {move_failed, Reason} ->
            io:format("Bot ~p: Move failed - ~p~n", [PlayerNumber, Reason]);
        {damage_taken, NewLife} ->
            io:format("Bot ~p: Took damage! Lives: ~p~n", [PlayerNumber, NewLife]);
        player_died ->
            io:format("Bot ~p: DIED!~n", [PlayerNumber]);
        {error, Reason} ->
            io:format("Bot ~p: Error - ~p~n", [PlayerNumber, Reason]);
        _ ->
            ok % Don't log other responses
    end.