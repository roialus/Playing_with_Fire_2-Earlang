%%%-------------------------------------------------------------------
%%% @author dolev
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Jul 2025 11:19
%%%-------------------------------------------------------------------
-module(gn_server).
-author("dolev").

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(gn_state, {
    tiles_table_name,
    bombs_table_name,
    powerups_table_name,
    players_table_name
}).

-include("mnesia_records.hrl").
-include_lib("/home/dolev/Documents/Erlang_project/Code/Objects/object_records.hrl"). %% todo: need to find a way around full-path



%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(GN_number::integer(), PlayerType::atom('bot'|'human')) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(GN_number, PlayerType) ->
    %% PlayerType = bot/human
    GN_name = list_to_atom("GN_" ++ integer_to_list(GN_number)),
    gen_server:start_link({global, GN_name}, ?MODULE, [GN_number, PlayerType], []). % register GN names GLOBALLY

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
{ok, State :: #gn_state{}} | {ok, State :: #gn_state{}, timeout() | hibernate} |
{stop, Reason :: term()} | ignore).
init([GN_number, PlayerType]) ->
    Data = #gn_state{
        tiles_table_name = generate_atom_table_names(GN_number, "_tiles"),
        bombs_table_name = generate_atom_table_names(GN_number, "_bombs"),
        powerups_table_name = generate_atom_table_names(GN_number, "_powerups"),
        players_table_name = generate_atom_table_names(GN_number, "_players")},
    initialize_tiles(Data#gn_state.tiles_table_name),
    initialize_players(Data#gn_state.players_table_name, PlayerType), % todo
    {ok, Data}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #gn_state{}) ->
{reply, Reply :: term(), NewState :: #gn_state{}} |
{reply, Reply :: term(), NewState :: #gn_state{}, timeout() | hibernate} |
{noreply, NewState :: #gn_state{}} |
{noreply, NewState :: #gn_state{}, timeout() | hibernate} |
{stop, Reason :: term(), Reply :: term(), NewState :: #gn_state{}} |
{stop, Reason :: term(), NewState :: #gn_state{}}).
handle_call(_Request, _From, State = #gn_state{}) ->
{reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #gn_state{}) ->
{noreply, NewState :: #gn_state{}} |
{noreply, NewState :: #gn_state{}, timeout() | hibernate} |
{stop, Reason :: term(), NewState :: #gn_state{}}).
handle_cast(_Request, State = #gn_state{}) ->
{noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #gn_state{}) ->
{noreply, NewState :: #gn_state{}} |
{noreply, NewState :: #gn_state{}, timeout() | hibernate} |
{stop, Reason :: term(), NewState :: #gn_state{}}).
handle_info(_Info, State = #gn_state{}) ->
{noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #gn_state{}) -> term()).
terminate(_Reason, _State = #gn_state{}) ->
ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #gn_state{},
Extra :: term()) ->
{ok, NewState :: #gn_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #gn_state{}, _Extra) ->
{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% helper function to create mnesia table names
generate_atom_table_names(Number, Type) ->
    list_to_atom("gn" ++ integer_to_list(Number) ++ Type).

initialize_tiles(TableName) ->
    Fun = fun() ->
        AllRecords = mnesia:match_object(TableName, #mnesia_tiles{_ = '_'}, read),
        lists:foreach(
            fun(Tile) ->
            % spawn a tile gen_server process for each tile in the table
                [Pos_x, Pos_y] = Tile#mnesia_tiles.position,
                [Type, Contains] = [Tile#mnesia_tiles.type, Tile#mnesia_tiles.contains],
                {ok, Pid} = tile:start_link(Pos_x, Pos_y, Type, Contains),
                UpdatedRecord = Tile#mnesia_tiles{pid = Pid},
                mnesia:write(TableName, UpdatedRecord, write)
                end,
            AllRecords), ok
        end,
    mnesia:activity(transaction, Fun).

initialize_players(TableName, PlayerType) -> ok. % TODO
