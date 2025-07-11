%%%-------------------------------------------------------------------
%%% @author dolev
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jul 2025 21:12
%%%-------------------------------------------------------------------
-module(powerup).
-author("dolev").

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-include("common_parameters.hrl").
-include("object_records.hrl").


%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(Pos_x::integer, Pos_y::integer,
    Type:: 'movespeed'|'regular_bomb'|'remote_bomb'|'repeating_bomb'|
        'bomb_kick'|'freeze_bomb'|'move_through_bomb'|'more_bombs'|'range'|
        'extra_life') ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Pos_x, Pos_y, Type) ->
    Server_name = list_to_atom("powerup_" ++ integer_to_list(Pos_x) ++ "_" ++ integer_to_list(Pos_y)),
    % registers *locally* as atom called 'tile_X_Y' (X,Y - numbers indicating location)
    % TODO: maybe there's no need to register, but just hold at the GN a database of the position, type and Pid of tiles
    gen_server:start_link({local, Server_name}, ?MODULE, [[Pos_x, Pos_y], Type, node()], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
    {ok, State :: #powerup_state{}} | {ok, State :: #powerup_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([Position, Type, Node_ID]) ->
    {ok, #powerup_state{position=Position, type=Type, original_node_ID=Node_ID}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
        State :: #powerup_state{}) ->
    {reply, Reply :: term(), NewState :: #powerup_state{}} |
    {reply, Reply :: term(), NewState :: #powerup_state{}, timeout() | hibernate} |
    {noreply, NewState :: #powerup_state{}} |
    {noreply, NewState :: #powerup_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #powerup_state{}} |
    {stop, Reason :: term(), NewState :: #powerup_state{}}).
handle_call(Request, _From, State = #powerup_state{}) ->
    case Request of
        pickup -> % pick up power-up, terminate process and return power-up type
            {stop, State#powerup_state.type, State};
        _ -> % todo: catch-all, don't have any other interaction I can think of for now
            {stop, catchall_clause, State}
    end.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #powerup_state{}) ->
    {noreply, NewState :: #powerup_state{}} |
    {noreply, NewState :: #powerup_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #powerup_state{}}).
handle_cast(_Request, State = #powerup_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #powerup_state{}) ->
    {noreply, NewState :: #powerup_state{}} |
    {noreply, NewState :: #powerup_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #powerup_state{}}).
handle_info(_Info, State = #powerup_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
        State :: #powerup_state{}) -> term()).
terminate(_Reason, _State = #powerup_state{}) -> ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #powerup_state{},
        Extra :: term()) ->
    {ok, NewState :: #powerup_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #powerup_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
