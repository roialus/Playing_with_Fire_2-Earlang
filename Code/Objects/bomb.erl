%% ? this file isn't used, just to supress errors
-compile(nowarn_unused_function).
-compile(nowarn_unused_vars).

%%%-------------------------------------------------------------------
%%% @author dolev
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% Interactions:
%%% all outside interaction go through handle_call (sync.)
%%% ** possible received messages: (input -> output) **
%%% ignite -> frozen_ignited | explode_next_tick | not_remote
%%% hit_by_explosion -> explode_next_tick
%%% freeze -> frozen | frozen_ignited | refreeze | unexpected_call (error-catch)
%%% {kick, Direction} -> started_movement
%%% {req_move, granted} -> continue_movement
%%% {req_move, denied} -> stopped_movement
%%%
%%% Explosion mechanism:
%%% self-message triggers the explosion (can be delayed or triggered prematurely)
%%% exit reason is {exploded, State#bomb_state.radius},
%%% *for this to work well, parent process (GN) should monitor the bomb process
%%% @end
%%% Created : 04. Jul 2025 12:26
%%%-------------------------------------------------------------------
-module(bomb).
-author("dolev").
-behaviour(gen_server).

%% API
-export([start_link/5]). % TODO: should probably be changed to start_monitor

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(TICK, 50). % macro for tick-time - delay between ticks
-define(FREEZE_DELAY, 3000). % macro for delay added when bomb is frozen
-define(EXPLODE_DELAY, 3000). % macro for delay for normal explosions
-define(MOVE_HALF_TILE, 500). % time to reach the 'border' between 2 tiles 
-define(FULL_MOVEMENT, 1000). % time to reach the middle of the target tile

-record(bomb_state, {
    type, % type of bomb - regular / remote / repeating
    ignited = false, % if not ignited - holds 'false', if ignited - holds a ref to the timer behind the self msg
    status = normal, % can be - normal / frozen
    time_placed, % time at which the bomb was placed, given by GN
    radius = 1, % blast radius on a + shape - number is how many blocks away the explosion is felt
    position, % position - [X,Y]
    movement = [0,0], % speed - [x_axi, y_axi]
    owner = none, % player name/ID (?) of whoever placed the bomb. 'none' is for a bomb that fell from a broken tile (or simply no owner)
    gn_pid, % GN Pid who oversees this process
    original_node_ID % original creating node ID - TODO: unsure of necessity
}).


%%%===================================================================
%%% API
%%%===================================================================


%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(Pos_x::integer, Pos_y::integer,
    Type:: 'regular'|'remote'|'repeating',
    Time_created:: integer(),
    Optional:: list()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).

start_link(Pos_x, Pos_y, Type, Time_created, Optional) ->
    gen_server:start_link({local}, ?MODULE, [[Pos_x, Pos_y], Type, Time_created, node(), self(), Optional], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server

-spec(init(Args :: term()) ->
    {ok, State :: #bomb_state{}} | {ok, State :: #bomb_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).

init([Position, Type, Time_created, Node_ID, GN_Pid, [] ]) -> % no optional data (no owner)
    State = case Type of 
        remote ->
            #bomb_state{position=Position, type=Type, ignited=false, time_placed=Time_created, original_node_ID=Node_ID, gn_pid=GN_Pid};
        _ ->
            Timer_ref = erlang:send_after(?EXPLODE_DELAY, self(), explode), % send self-message to explode in 3 seconds
            #bomb_state{position=Position, type=Type, ignited=Timer_ref, time_placed=Time_created, original_node_ID=Node_ID, gn_pid=GN_Pid}
    end,
    {ok, State};

init([Position, Type, Time_created, Node_ID, [Owner_ID, Radius]]) -> % with optional data (owner, radius)
    State = case Type of 
        remote ->
            #bomb_state{radius=Radius, owner=Owner_ID, position=Position, type=Type, ignited=false, time_placed=Time_created, original_node_ID=Node_ID};
        _ ->
            Timer_ref = erlang:send_after(?EXPLODE_DELAY, self(), explode), % send self-message to explode in 3 seconds
            #bomb_state{radius=Radius, owner=Owner_ID, position=Position, type=Type, ignited=Timer_ref, time_placed=Time_created, original_node_ID=Node_ID}
    end,
    {ok, State}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #bomb_state{}) ->
    {reply, Reply :: term(), NewState :: #bomb_state{}} |
    {reply, Reply :: term(), NewState :: #bomb_state{}, timeout() | hibernate} |
    {noreply, NewState :: #bomb_state{}} |
    {noreply, NewState :: #bomb_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #bomb_state{}} |
    {stop, Reason :: term(), NewState :: #bomb_state{}}).


handle_call(Request, _From, State = #bomb_state{}) ->
    case Request of
        ignite -> if %% ignite message received
            State#bomb_state.type == remote -> % bomb type = remote
                case State#bomb_state.status of
                    normal -> % bomb not frozen, set to explode next game-tick
                        Timer_ref = erlang:send_after(?TICK, self(), explode),
                        New_state = State#bomb_state{ignited=Timer_ref},
                        {reply, explode_next_tick, New_state};
                    frozen -> % bomb frozen, will explode in 3 seconds
                        Timer_ref = erlang:send_after(?FREEZE_DELAY, self(), explode),
                        New_state = State#bomb_state{ignited=Timer_ref},
                        {reply, frozen_ignited, New_state}
                end;
            true -> % bomb type = regular/repeating - nothing to ignite
                {reply, not_remote, State}
            end;
            
        hit_by_explosion -> % another bomb exploded and the explosion reached this bomb
            erlang:send_after(?TICK, self(), hit_by_explosion), % explodes next tick
            {reply, explode_next_tick, State};
        freeze -> % a freezing action was applied to the bomb
            case {State#bomb_state.type, State#bomb_state.ignited} of
                {remote, false} -> % bomb type = remote, not yet ignited
                    New_state = State#bomb_state{status=frozen},
                    {reply, frozen, New_state};
                {remote, Current_timer_ref} -> % bomb type = remote, already ignited
                    %% disables current timer, adds +3 seconds to the explosions   
                    case erlang:cancel_timer(Current_timer_ref) of % "disable" previous explosion
                        true -> ok;
                        false -> 
                            receive
                                exploded -> ok
                            after 0 -> ok
                            end
                    end,
                    New_timer_ref = erlang:send_after(?FREEZE_DELAY, self(), exploded), 
                    New_state = State#bomb_state{status=frozen, ignited=New_timer_ref},
                    {reply, frozen_ignited, New_state};
                {_, Current_timer_ref} -> % bomb type = regular/repeating, already ignited
                    if
                        State#bomb_state.status == frozen -> % can't re-freeze a bomb, ignore action
                            {reply, refreeze, State};
                        true -> % bomb not frozen before
                            case erlang:cancel_timer(Current_timer_ref) of % "disable" previous explosion
                                true -> ok;
                                false -> 
                                    receive
                                        exploded -> ok
                                    after 0 -> ok
                                    end
                            end,
                            New_timer_ref = erlang:send_after(?FREEZE_DELAY, self(), exploded),
                            New_state = State#bomb_state{status=frozen, ignited=New_timer_ref},
                            {reply, frozen_ignited, New_state}
                    end;
                {Type,Ignited} -> % catch-all for errors
                    {stop, {unexpected_call, Type, Ignited}, State}
            end;
        {kick, Direction} -> % a player 'hit' the bomb with the proper buff, Direction is the movement direction of the player @ moment of hitting
        %% UNLIKE player process dynamics, the bomb is cleared to move in this direction to the next position
        %% when it reaches the next tile it asks for permissions to move to the next one, and so on.
            New_state = State#bomb_state{movement=Direction},
            erlang:send_after(?MOVE_HALF_TILE, self(), update_pos),
            erlang:send_after(?FULL_MOVEMENT, self(), req_further_move),
            {reply, started_movement, New_state};

        {req_move, granted} -> % further movement is granted
            erlang:send_after(?MOVE_HALF_TILE, self(), update_pos),
            erlang:send_after(?FULL_MOVEMENT, self(), req_further_move),
            {reply, continue_movement, State};

        {req_move, denied} -> % further movement is denied - stop in current position
            New_state = State#bomb_state{movement=[0,0]}, % reset movement
            {reply, stopped_movement, New_state}


        % NO BASE CASE - todo: necessary? add later?
    end.

%% ----------------------------------------------------------------------

%% @private
%% @doc Handling cast messages - async. messaging
-spec(handle_cast(Request :: term(), State :: #bomb_state{}) ->
    {noreply, NewState :: #bomb_state{}} |
    {noreply, NewState :: #bomb_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #bomb_state{}}).

handle_cast(_Request, State = #bomb_state{}) ->
    {noreply, State}.

%% ----------------------------------------------------------------------

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #bomb_state{}) ->
    {noreply, NewState :: #bomb_state{}} |
    {noreply, NewState :: #bomb_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #bomb_state{}}).

handle_info(Info, State = #bomb_state{}) ->
    case Info of
        hibernate ->
            {noreply, State, hibernate};

        update_pos -> % reached the halfway point of the movement, update position and notify GN
            NewPos = lists:zipwith(fun(X,Y) -> X+Y end, State#bomb_state.position, State#bomb_state.movement),
            New_state = State#bomb_state{position=NewPos},
            erlang:send(State#bomb_state.gn_pid, {updated_pos, NewPos, self()}),
            {noreply, New_state};

        req_further_move -> % reached middle of next tile, ask GN for permission to keep going
            Next_pos = lists:zipwith(fun(X,Y) -> X+Y end, State#bomb_state.position, State#bomb_state.movement),
            erlang:send(State#bomb_state.gn_pid, {req_move, Next_pos, self()}),
            {noreply, State};

        hit_by_explosion -> % hit by another bomb's explosions
            if
                State#bomb_state.type == repeating -> % repeating type - tells GN to create another (regular) bomb at location
                    {stop, {exploded_repeating, State#bomb_state.radius}, State};
                true -> % not repeating bomb - just explodes and be done with it
                    {stop, {exploded, State#bomb_state.radius}, State}
            end;

        explode -> % exploding based on internal timer/mechanism
            if
                State#bomb_state.type == repeating -> % repeating type - tells GN to create another (regular) bomb at location
                    {stop, {exploded_repeating, State#bomb_state.radius}, State};
                true -> % not repeating bomb - just explodes and be done with it
                    {stop, {exploded, State#bomb_state.radius}, State}
            end;
        
        _ -> {noreply, State}
    end.
%% ----------------------------------------------------------------------


%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #bomb_state{}) -> term()).

terminate(_Reason, _State = #bomb_state{}) -> ok.


%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #bomb_state{},
    Extra :: term()) ->
    {ok, NewState :: #bomb_state{}} | {error, Reason :: term()}).

code_change(_OldVsn, State = #bomb_state{}, _Extra) ->
    {ok, State}.



%%%===================================================================
%%% Internal functions
%%%===================================================================