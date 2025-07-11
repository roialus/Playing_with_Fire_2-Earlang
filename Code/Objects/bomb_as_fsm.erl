%%%-------------------------------------------------------------------
%%% @author dolev
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jul 2025 15:41
%%%-------------------------------------------------------------------
-module(bomb_as_fsm).
-author("dolev").

-behaviour(gen_statem).

%% API
-export([start_link/4]). % todo: check whether start_link/start_monitor is desired

%% gen_statem callbacks
-export([init/1, format_status/2, terminate/3,
    code_change/4, callback_mode/0]).

-export([remote_idle/3, armed/3, active_movement/3, delayed_explosion_state/3,
    remote_armed/3, remote_idle_movement/3, remote_armed_frozen_movement/3]).

%% Parameters Definitions
-define(SERVER, ?MODULE).
-define(EXPLODE_DELAY, 3000).
-define(FREEZE_DELAY, 2000).
-define(TICK_DELAY, 50).
-define(HALFWAY_TILE, 350). % time to complete half the movement - the switching point between tiles

-record(bomb_state, {
    type, % type of bomb - regular/remote/repeating(W.I.P - not yet implemented)
    ignited = false, % bomb ignition status - can be false or {true, TimerRef}
    status = normal, % normal/frozen
    radius = 1, % radius of explosion - number means how many tiles in each direction in + shape
    position, % location of bomb, [X,Y]
    direction = none, % desired moving direction, can be - none/up/down/left/right
    movement = false, % can be - false / {true, TimerRef}
    owner, % owner of the bomb - can be none or player ID (not pid)
    gn_pid % GN pid who oversees this process
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link(Pos_x, Pos_y, Type, Optional) ->
    %% optional is a list containing [Player_ID, Radius]
    %% bomb is nameless - identified based on Pid (**consider changing if needed**)
    gen_statem:start_link({local}, ?MODULE, [[Pos_x, Pos_y], Type, self(), Optional], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init([Position, Type, Gn_Pid, Optional]) ->
    StateData = case Optional of
        [] -> % no owner, default radius
            #bomb_state{position = Position,
                        type = Type,
                        gn_pid = Gn_Pid,
                        owner = none};
        [Player_ID] -> % stated owner, default radius
            #bomb_state{position = Position,
                        type = Type,
                        gn_pid = Gn_Pid,
                        owner = Player_ID };
        [Player_ID, Radius] -> % stated owner, custom radius
            #bomb_state{position = Position,
                        type = Type,
                        gn_pid = Gn_Pid,
                        owner = Player_ID,
                        radius = Radius }
    end,
    case StateData#bomb_state.type of
        regular ->
            UpdatedData = StateData#bomb_state{ignited = {true, erlang:system_time(millisecond)}},
            {ok, armed, UpdatedData, [{state_timeout, ?EXPLODE_DELAY, explode}]};
        remote -> remote_idle; % TODO
        repeating -> % repeating bomb, w.i.p
            UpdatedData = StateData#bomb_state{ignited = {true, erlang:system_time(millisecond)}},
            {ok, armed, UpdatedData, [{state_timeout, ?EXPLODE_DELAY, explode}]}
    end.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
    [state_functions, state_enter].

%% @private
%% @doc Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
format_status(_Opt, [_PDict, _StateName, _State]) ->
    Status = some_term,
    Status.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(normal, _CurrentState, StateData = #bomb_state{}) ->
    StateData#bomb_state.gn_pid ! {bomb_exploded, self()},
    ok;

terminate(_Reason, _StateName, _State = #bomb_state{}) ->
    ok.



%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #bomb_state{}, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% state functions
%%%===================================================================
%% ~~~~~~~~~ State = armed ~~~~~~~~~
%% Syntax for state enter actions - not used here, but syntax is valid
%armed(enter, _OldState, StateData = #bomb_state{}) ->
%    UpdatedData = StateData#bomb_state{ignited = {true, erlang:system_time(millisecond)}},
%    {keep_state, UpdatedData, [{state_timeout, ?EXPLODE_DELAY, explode}]};

armed(cast, freeze, StateData = #bomb_state{}) ->
    %% if already frozen - do nothing.
    %% else - add FREEZE_DELAY to the timeout timer (fetch old time, calc. leftover + FREEZE_DELAY)
    if
        StateData#bomb_state.status == frozen -> {keep_state_and_data};
        true ->
            UpdatedData = StateData#bomb_state{status=frozen},
            TempTime = calc_new_explode_delay(StateData),
            {keep_state, UpdatedData, [{state_timeout, TempTime - erlang:system_time(millisecond), explode}]}
    end;

armed({call, GN}, {kick, Direction}, StateData = #bomb_state{}) ->
    %% message from GN received - kicked by player.
    %% Reply with request for movement in said direction: {request_movement, Direction}
    %% remain in the same state until answered, retain state_timeout
    {keep_state, StateData#bomb_state{direction = Direction}, [{reply, GN, {request_movement, Direction}}]};

armed(cast, {reply_move_req, Answer}, StateData = #bomb_state{}) ->
    %% received a reply from GN about the movement request
    %% answer can be 1 of 3: 'approved', {'approved_switch_gn', NewGN}, 'denied'
    case Answer of
        approved -> % movement approved
            TempTime = calc_new_explode_delay(StateData),
            {next_state, active_movement, StateData, [{state_timeout, TempTime - erlang:system_time(millisecond), explode}]};
        denied -> % movement denied
            {keep_state, StateData#bomb_state{direction = none}};
        {approved_switch_gn, _NewGN} -> % movement approved, need to switch GNs
            {keep_state_and_data} % TODO: not implemented yet
    end;

armed(cast, damage_taken, StateData = #bomb_state{}) ->
    %% explodes after one time TICK
    {next_state, delayed_explosion_state, StateData, [{state_timeout, ?TICK_DELAY, external_explode}]};

armed(cast, ignite, StateData = #bomb_state{}) ->
    %% wrong bomb type, ignore it
    {keep_state, StateData};

armed(state_timeout, _Reason, StateData = #bomb_state{}) ->
    %% bomb timeout handler - bomb is exploding
    {stop, normal, StateData};

%% unknown messages - stop the process with an error
armed(cast, _Message, StateData = #bomb_state{}) ->
    {stop, unsupported_message_in_state, StateData};

armed({call, GN}, _Message, StateData = #bomb_state{}) ->
    {stop, unsupported_message_in_state, StateData, {reply, GN, 'FSM_error'}}.

%% ~~~~~~~~~ State = active_movement ~~~~~~~~~

active_movement(enter, _OldState, StateData = #bomb_state{}) ->
    %% entering to this state from any other state
    UpdatedData = StateData#bomb_state{movement = {true, erlang:send_after(?HALFWAY_TILE, self(), halfway_timer)}},
    {keep_state, UpdatedData};

active_movement(state_timeout, _Reason, StateData = #bomb_state{}) ->
    %% bomb timeout handler - bomb is exploding
    {stop, normal, StateData};

active_movement(cast, freeze, StateData = #bomb_state{}) ->
    %% if already frozen - do nothing.
    %% else - add FREEZE_DELAY to the timeout timer (fetch old time, calc. leftover + FREEZE_DELAY)
    if
    StateData#bomb_state.status == frozen -> {keep_state_and_data};
    true ->
    UpdatedData = StateData#bomb_state{status=frozen},
    TempTime = calc_new_explode_delay(StateData),
    {keep_state, UpdatedData, [{state_timeout, TempTime - erlang:system_time(millisecond), explode}]}
    end;

active_movement({call, GN}, {kick, _Direction}, StateData = #bomb_state{}) ->
    %% Shouldn't exist in this state. The bomb first needs to collide
    {stop, unsupported_message_in_state, StateData, {reply, GN, 'FSM_error'}};

active_movement(cast, stop_movement, StateData = #bomb_state{}) ->
    %% External stop movement (i.e. player collided into it).
    %% update records, move to 'armed' state, cancel movement timer
    case StateData#bomb_state.movement of
        {true, TimerRef} -> erlang:cancel_timer(TimerRef);
        false -> ok
    end,
    UpdatedData = StateData#bomb_state{movement = false, direction = none},
    TempTime = calc_new_explode_delay(StateData),
    {next_state, armed, UpdatedData, [{state_timeout, TempTime - erlang:system_time(millisecond), explode}]};

active_movement(info, Send_after_timers, StateData = #bomb_state{}) ->
    %% deals with erlang:send_after timers
    case Send_after_timers of
        halfway_timer -> % halfway point timer
            UpdatedData = StateData#bomb_state{
                position = new_position(StateData#bomb_state.position, StateData#bomb_state.direction),
                movement = {true, erlang:send_after(?HALFWAY_TILE, self(), full_tile_change)}
                },
            UpdatedData#bomb_state.gn_pid ! {updated_position, UpdatedData#bomb_state.position}, % message GN with new position
            {keep_state, UpdatedData};
        full_tile_change -> % finished a full movement, checking if we can continue moving
            StateData#bomb_state.gn_pid ! {request_movement, StateData#bomb_state.direction},
            {keep_state, StateData#bomb_state{movement = false}} % currently not moving, awaiting reply
    end;

active_movement(cast, {reply_move_req, Answer}, StateData = #bomb_state{}) ->
    %% received a reply from GN about the movement request
    %% answer can be 1 of 3: 'approved', {'approved_switch_gn', NewGN}, 'denied'
    case Answer of
        approved -> % movement approved
            UpdatedData = StateData#bomb_state{movement = {true, erlang:send_after(?HALFWAY_TILE, self(), halfway_timer)}},
            {keep_state, UpdatedData};
        denied -> % movement denied
            UpdatedData = StateData#bomb_state{movement = false, direction = none},
            TempTime = calc_new_explode_delay(StateData),
            {next_state, armed, UpdatedData, [{state_timeout, TempTime - erlang:system_time(millisecond), explode}]};
        {approved_switch_gn, _NewGN} -> % movement approved, need to switch GNs
            {keep_state_and_data} % TODO: not implemented yet
    end;

active_movement(cast, damage_taken, StateData = #bomb_state{}) ->
    %% damage taken. blow up in the next time tick.
    %% To avoid collision with movement timers - switches to a state which only times-out into explosion
    {next_state, delayed_explosion_state, StateData, [{state_timeout, ?TICK_DELAY, external_explode}]};

active_movement(cast, ignite, StateData = #bomb_state{}) ->
    %% wrong bomb type, ignore it
    {keep_state, StateData}.

%% ~~~~~~~~~ State = delayed_explosion_state ~~~~~~~~~
%% Throwaway state to ignore everything but state_timeout to explode

delayed_explosion_state(info, _AnyMessage, StateData = #bomb_state{}) ->
    {keep_state, StateData};

delayed_explosion_state(state_timeout, _Reason, StateData = #bomb_state{}) ->
    %% bomb timeout handler - bomb is exploding
    {stop, normal, StateData}.

%% todo: add ignoring for other messages (cast,call)? idk

%% ~~~~~~~~~ State = remote_idle ~~~~~~~~~

remote_idle(cast, freeze, StateData = #bomb_state{}) ->
        {keep_state, StateData#bomb_state{status=frozen}};

remote_idle({call, GN}, {kick, Direction}, StateData = #bomb_state{}) ->
    %% message from GN received - kicked by player.
    %% Reply with request for movement in said direction: {request_movement, Direction}
    %% remain in the same state until answered, retain state_timeout
    {keep_state, StateData#bomb_state{direction = Direction}, [{reply, GN, {request_movement, Direction}}]};

remote_idle(cast, {reply_move_req, Answer}, StateData = #bomb_state{}) ->
    %% received a reply from GN about the movement request
    %% answer can be 1 of 3: 'approved', {'approved_switch_gn', NewGN}, 'denied'
    case Answer of
        approved -> % movement approved
            {next_state, remote_idle_movement, StateData};
        denied -> % movement denied
            {keep_state, StateData#bomb_state{direction = none}};
        {approved_switch_gn, _NewGN} -> % movement approved, need to switch GNs
            {keep_state_and_data} % TODO: not implemented yet
    end;

remote_idle(cast, damage_taken, StateData = #bomb_state{}) ->
    %% damage taken. blow up in the next time tick.
    {next_state, delayed_explosion_state, StateData, [{state_timeout, ?TICK_DELAY, external_explode}]};

remote_idle(cast, ignite, StateData = #bomb_state{}) ->
    %% wrong bomb type, ignore it
    if
        StateData#bomb_state.status == frozen -> % bomb is frozen and armed,
            UpdatedData = StateData#bomb_state{ignited = {true, erlang:system_time(millisecond)}},
            {next_state, remote_armed, UpdatedData, [{state_timeout, ?FREEZE_DELAY, explode}]};
        true ->
            {stop, normal, StateData}
    end.

% todo: add addressing/ignoring for everything not in here? (cast,call,info) idk

%% ~~~~~~~~~ State = remote_idle ~~~~~~~~~

remote_armed(state_timeout, _Reason, StateData = #bomb_state{}) ->
    %% bomb timeout handler - bomb is exploding
    {stop, normal, StateData};

remote_armed(cast, freeze, StateData = #bomb_state{}) ->
    %% Already frozen, ignore this
    {keep_state, StateData};

remote_armed({call, GN}, {kick, Direction}, StateData = #bomb_state{}) ->
    %% message from GN received - kicked by player.
    %% Reply with request for movement in said direction: {request_movement, Direction}
    %% remain in the same state until answered, retain state_timeout
    {keep_state, StateData#bomb_state{direction = Direction}, [{reply, GN, {request_movement, Direction}}]};

remote_armed(cast, {reply_move_req, Answer}, StateData = #bomb_state{}) ->
    %% received a reply from GN about the movement request
    %% answer can be 1 of 3: 'approved', {'approved_switch_gn', NewGN}, 'denied'
    case Answer of
        approved -> % movement approved
            TempTime = calc_new_explode_delay(StateData),
            {next_state, remote_armed_frozen_movement, StateData, [{state_timeout, TempTime - erlang:system_time(millisecond), explode}]};
        denied -> % movement denied
            {keep_state, StateData#bomb_state{direction = none}};
        {approved_switch_gn, _NewGN} -> % movement approved, need to switch GNs
            {keep_state_and_data} % TODO: not implemented yet
    end;

remote_armed(cast, damage_taken, StateData = #bomb_state{}) ->
    %% explodes after one time TICK
    {next_state, delayed_explosion_state, StateData, [{state_timeout, ?TICK_DELAY, external_explode}]};

remote_armed(cast, ignite, StateData = #bomb_state{}) ->
    %% Already armed, ignore
    {keep_state, StateData};

%% unknown messages - stop the process with an error
remote_armed(cast, _Message, StateData = #bomb_state{}) ->
    {stop, unsupported_message_in_state, StateData};

remote_armed({call, GN}, _Message, StateData = #bomb_state{}) ->
    {stop, unsupported_message_in_state, StateData, {reply, GN, 'FSM_error'}}.


%% ~~~~~~~~~ State = remote_idle_movement ~~~~~~~~~

remote_idle_movement(enter, _OldState, StateData = #bomb_state{}) ->
    %% entering to this state from any other state
    UpdatedData = StateData#bomb_state{movement = {true, erlang:send_after(?HALFWAY_TILE, self(), halfway_timer)}},
    {keep_state, UpdatedData};

remote_idle_movement(cast, freeze, StateData = #bomb_state{}) ->
    {keep_state, StateData#bomb_state{status = frozen}};

remote_idle_movement({call, GN}, {kick, _Direction}, StateData = #bomb_state{}) ->
    %% Shouldn't exist in this state. The bomb first needs to collide
    {stop, unsupported_message_in_state, StateData, {reply, GN, 'FSM_error'}};

remote_idle_movement(cast, stop_movement, StateData = #bomb_state{}) ->
    %% External stop movement (i.e. player collided into it).
    %% update records, move to 'armed' state, stop movement timer
    case StateData#bomb_state.movement of
        {true, TimerRef} -> erlang:cancel_timer(TimerRef);
        false -> ok
    end,
    UpdatedData = StateData#bomb_state{movement = false, direction = none},
    {next_state, remote_idle, UpdatedData};

remote_idle_movement(info, Send_after_timers, StateData = #bomb_state{}) ->
    %% deals with erlang:send_after timers
    case Send_after_timers of
        halfway_timer -> % halfway point timer
            UpdatedData = StateData#bomb_state{
                position = new_position(StateData#bomb_state.position, StateData#bomb_state.direction),
                movement = {true, erlang:send_after(?HALFWAY_TILE, self(), full_tile_change)}
            },
            UpdatedData#bomb_state.gn_pid ! {updated_position, UpdatedData#bomb_state.position}, % message GN with new position
            {keep_state, UpdatedData};
        full_tile_change -> % finished a full movement, checking if we can continue moving
            StateData#bomb_state.gn_pid ! {request_movement, StateData#bomb_state.direction},
            {keep_state, StateData#bomb_state{movement = false}} % currently not moving, awaiting reply
    end;

remote_idle_movement(cast, {reply_move_req, Answer}, StateData = #bomb_state{}) ->
    %% received a reply from GN about the movement request
    %% answer can be 1 of 3: 'approved', {'approved_switch_gn', NewGN}, 'denied'
    case Answer of
        approved -> % movement approved
            UpdatedData = StateData#bomb_state{movement = {true, erlang:send_after(?HALFWAY_TILE, self(), halfway_timer)}},
            {keep_state, UpdatedData};
        denied -> % movement denied
            UpdatedData = StateData#bomb_state{movement = false, direction = none},
            {next_state, remote_idle, UpdatedData};
        {approved_switch_gn, _NewGN} -> % movement approved, need to switch GNs
            {keep_state_and_data} % TODO: not implemented yet
    end;

remote_idle_movement(cast, damage_taken, StateData = #bomb_state{}) ->
    %% damage taken. blow up in the next time tick.
    %% To avoid collision with movement timers - switches to a state which only times-out into explosion
    {next_state, delayed_explosion_state, StateData, [{state_timeout, ?TICK_DELAY, external_explode}]};

remote_idle_movement(cast, ignite, StateData = #bomb_state{}) ->
    %% if status is normal - explode immediately
    %% else (frozen) - update record, start state_timeout timer & switch to remote_armed_frozen_movement
    case StateData#bomb_state.status of
        normal -> % not frozen, explode immediately
            {stop, normal, StateData};
        frozen -> % frozen, update record, start timer, switch states
            UpdatedData = StateData#bomb_state{ignited = {true, erlang:system_time(millisecond)}},
            {next_state, remote_armed_frozen_movement, UpdatedData, [{state_timeout, ?FREEZE_DELAY, explode}]}
    end.

%% ~~~~~~~~~ State = remote_armed_frozen_movement ~~~~~~~~~

remote_armed_frozen_movement(state_timeout, _Reason, StateData = #bomb_state{}) ->
    %% bomb timeout handler - bomb is exploding
    {stop, normal, StateData};

remote_armed_frozen_movement(cast, freeze, StateData = #bomb_state{}) ->
    %% already frozen, change nothing
    {keep_state, StateData};

remote_armed_frozen_movement({call, GN}, {kick, _Direction}, StateData = #bomb_state{}) ->
    %% Shouldn't exist in this state. The bomb first needs to collide
    {stop, unsupported_message_in_state, StateData, {reply, GN, 'FSM_error'}};

remote_armed_frozen_movement(cast, stop_movement, StateData = #bomb_state{}) ->
    %% External stop movement (i.e. player collided into it).
    %% update records, move to remote_armed state, stop movement timer, pass state_timeout along
    case StateData#bomb_state.movement of
        {true, TimerRef} -> erlang:cancel_timer(TimerRef);
        false -> ok
    end,
    TempTime = calc_new_explode_delay(StateData),
    UpdatedData = StateData#bomb_state{movement = false, direction = none},
    {next_state, remote_armed, UpdatedData, [{state_timeout, TempTime - erlang:system_time(millisecond), explode}]};

remote_armed_frozen_movement(info, Send_after_timers, StateData = #bomb_state{}) ->
    %% deals with erlang:send_after timers
    case Send_after_timers of
        halfway_timer -> % halfway point timer
            UpdatedData = StateData#bomb_state{
                position = new_position(StateData#bomb_state.position, StateData#bomb_state.direction),
                movement = {true, erlang:send_after(?HALFWAY_TILE, self(), full_tile_change)}
            },
            UpdatedData#bomb_state.gn_pid ! {updated_position, UpdatedData#bomb_state.position}, % message GN with new position
            {keep_state, UpdatedData};
        full_tile_change -> % finished a full movement, checking if we can continue moving
            StateData#bomb_state.gn_pid ! {request_movement, StateData#bomb_state.direction},
            {keep_state, StateData#bomb_state{movement = false}} % currently not moving, awaiting reply
    end;

remote_armed_frozen_movement(cast, {reply_move_req, Answer}, StateData = #bomb_state{}) ->
    %% received a reply from GN about the movement request
    %% answer can be 1 of 3: 'approved', {'approved_switch_gn', NewGN}, 'denied'
    case Answer of
        approved -> % movement approved
            UpdatedData = StateData#bomb_state{movement = {true, erlang:send_after(?HALFWAY_TILE, self(), halfway_timer)}},
            {keep_state, UpdatedData};
        denied -> % movement denied, stop manual timers, pass along left-over state_timeout
            case StateData#bomb_state.movement of
                {true, TimerRef} -> erlang:cancel_timer(TimerRef);
                false -> ok
            end,
            TempTime = calc_new_explode_delay(StateData),
            UpdatedData = StateData#bomb_state{movement = false, direction = none},
            {next_state, remote_armed, UpdatedData, [{state_timeout, TempTime - erlang:system_time(millisecond), explode}]};
        {approved_switch_gn, _NewGN} -> % movement approved, need to switch GNs
            {keep_state_and_data} % TODO: not implemented yet
    end;

remote_armed_frozen_movement(cast, damage_taken, StateData = #bomb_state{}) ->
    %% damage taken. blow up in the next time tick.
    %% To avoid collision with movement timers - switches to a state which only times-out into explosion
    {next_state, delayed_explosion_state, StateData, [{state_timeout, ?TICK_DELAY, external_explode}]};

remote_armed_frozen_movement(cast, ignite, StateData = #bomb_state{}) ->
    %% already armed, ignore this
    {keep_state, StateData}.



%%%===================================================================
%%% Internal functions
%%%===================================================================
calc_new_explode_delay(State = #bomb_state{}) ->
    {_, InitialIgnitionTime} = State#bomb_state.ignited,
    ?EXPLODE_DELAY + ?FREEZE_DELAY + InitialIgnitionTime.

new_position(Old_position, Direction) ->
    [X,Y] = Old_position,
    case Direction of
        up -> [X,Y+1];
        down -> [X,Y-1];
        left -> [X-1,Y];
        right -> [X+1,Y]
    end.