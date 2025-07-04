%%%-------------------------------------------------------------------
%%% @author dolev
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Jul 2025 12:26
%%%-------------------------------------------------------------------
-module(bomb).
-author("dolev").
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).


-record(bomb_state, {
    type, % type of bomb - regular / remote / repeating
    ignited = false, % if not ignited - holds 'false', if ignited - holds a ref to the timer behind the self msg
    status = normal, % can be - normal / frozen
    time_placed, % time at which the bomb was placed, given by GN
    radius = 1, % blast radius on a + shape - number is how many blocks away the explosion is felt
    position, % position - [X,Y]
    speed = [0,0], % speed - [x_axi, y_axi]
    owner = none, % player name/ID (?) of whoever placed the bomb. 'none' is for a bomb that fell from a broken tile (or simply no owner)
    original_node_ID % original creating node ID - TODO: unsure of necessity 
}).


%%%===================================================================
%%% API
%%%===================================================================


%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(Pos_x::integer, Pos_y::integer,
    Type:: 'regular'|'remote'|'repeating',
    Time_created:: time(), %todo: ??
    Optional:: list()) -> % todo: ?? 
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).

start_link(Pos_x, Pos_y, Type, Time_created, Optional) ->
    gen_server:start_link({local}, ?MODULE, [[Pos_x, Pos_y], Type, Time_created, node(), Optional], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server

-spec(init(Args :: term()) ->
    {ok, State :: #tile_state{}} | {ok, State :: #tile_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).

init([Position, Type, Time_created, Node_ID, [] ]) -> % no optional data (no owner)
    State = case Type of 
        remote -> #bomb_state{position=Position, type=Type, ignited=false, time_placed=Time_created, original_node_ID=Node_ID};
        _ -> Timer_ref = erlang:send_after(3000, self(), explode), % send self-message to explode in 3 seconds
            #bomb_state{position=Position, type=Type, ignited=Timer_ref, time_placed=Time_created, original_node_ID=Node_ID}
    end,
    erlang:send_after(0, self(), hibernate),
    {ok, State};

init([Position, Type, Time_created, Node_ID, [Owner_ID, Radius]]) -> % with optional data (owner, radius)
    State = case Type of 
        remote -> #bomb_state{radius=Radius, owner=Owner_ID, position=Position, type=Type, ignited=false, time_placed=Time_created, original_node_ID=Node_ID};
        _ -> Timer_ref = erlang:send_after(3000, self(), explode), % send self-message to explode in 3 seconds
            #bomb_state{radius=Radius, owner=Owner_ID, position=Position, type=Type, ignited=Timer_ref, time_placed=Time_created, original_node_ID=Node_ID}
    end,
    erlang:send_after(0, self(), hibernate),
    {ok, State}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #tile_state{}) ->
    {reply, Reply :: term(), NewState :: #tile_state{}} |
    {reply, Reply :: term(), NewState :: #tile_state{}, timeout() | hibernate} |
    {noreply, NewState :: #tile_state{}} |
    {noreply, NewState :: #tile_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #tile_state{}} |
    {stop, Reason :: term(), NewState :: #tile_state{}}).

% ** CONTINUE FROM HERE **

handle_call(Request, _From, State = #tile_state{}) ->
    % mechanism: stops itself if needs be, returns the 'contains' field
    case Request of
        hi -> case State#tile_state.type of
                     unbreakable -> % damage to unbreakable tile does nothing
                        {reply, unbreakable, State};
                     breakable -> % damage to breakable tile breaks it, letting the GN know about the 'drop'
                         {stop, normal, State#tile_state.contains ,State}; % calls terminate callback function
                     two_hit -> % moves to 2nd phase of breaking
                         New_State = State#tile_state{type = one_hit},
                         {reply, one_hit, New_State};
                    one_hit -> % being hit again - breaks the tile
                        {stop, normal, State#tile_state.contains, State} % calls terminate callback function, replies with 'contains'
                 end;
        terminate -> {stop, normal, State}
            % NO BASE CASE - todo: necessary? add later?
    end.

%% @private
%% @doc Handling cast messages - async. messaging
-spec(handle_cast(Request :: term(), State :: #tile_state{}) ->
    {noreply, NewState :: #tile_state{}} |
    {noreply, NewState :: #tile_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #tile_state{}}).

handle_cast(_Request, State = #tile_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #tile_state{}) ->
    {noreply, NewState :: #tile_state{}} |
    {noreply, NewState :: #tile_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #tile_state{}}).

handle_info(Info, State = #tile_state{}) ->
    case Info of
        hibernate -> {noreply, State, hibernate}; % todo: added a hibernation mode - maybe sends a message at the start to all tiles to sleep?
        _ -> {noreply, State}
    end.



%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #tile_state{}) -> term()).

terminate(_Reason, _State = #tile_state{}) -> ok.


%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #tile_state{},
    Extra :: term()) ->
    {ok, NewState :: #tile_state{}} | {error, Reason :: term()}).

code_change(_OldVsn, State = #tile_state{}, _Extra) ->
    {ok, State}.



%%%===================================================================
%%% Internal functions
%%%===================================================================