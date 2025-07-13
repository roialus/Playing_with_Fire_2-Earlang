%%%-------------------------------------------------------------------
%%% @author dolev
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Jul 2025 12:18
%%%-------------------------------------------------------------------
-module(cn_initial_startup).
-author("dolev").

%% API
-compile(export_all).
%-export([test/1, connecting_nodes/1]).

%% **NOTE:** when terminating, need to use application:stop(mnesia)

-include("mnesia_records.hrl").

%% --------------------------------------------------------------
%% ==================== Testing Functions =======================
%% todo: for super-massive debugging use sys:trace(Pid, true).
%% @doc connecting nodes according to an IP list
connecting_nodes(IPList) ->
    %% each erl shell will be called 'GN#@IP where IP is from the list.
    IPsAsAtoms = lists:foreach(
        fun(X) -> list_to_atom("GN" ++ integer_to_list(X) ++"@" ++ lists:nth(X, IPList)) end,
        lists:seq(1,length(IPList))),
    lists:map(
        fun(IP) -> io:format("Pinging ~w : ~w~n",[IP, net_adm:ping(IP)]) end, IPsAsAtoms),
    [node()] ++ IPsAsAtoms.


-spec test(list()) -> ok.
test(NodeList) ->
    %% NodeList = [node(), gn_node1, gn_node2, gn_node3, gn_node4] -- THIS IS HOW THIS LIST SHOULD LOOK LIKE
    Map = map_generator:test_generation(), % creating a new map from scratch
    application:set_env(mnesia, dir, "/Documents/mnesia_files"),
    mnesia:create_schema(NodeList),
    rpc:multicall(NodeList, application, start, [mnesia]), % multiple nodes
    % application:start(mnesia), % single node

    %% initialize mnesia tables per each game-node
    %% * This is the "degraded" version for simple testing - One CN node and one GN node.
    TableNamesList = lists:map(fun(X) ->
        create_tables(lists:nth(2, NodeList), node(), X) end, lists:seq(1,4)),

    %% * Full-fledged version - one CN, 4 GNs
    %%TableNamesList = lists:map(fun(X) ->
    %%        create_tables(lists:nth(X, NodeList), node(), X)
    %%    end, lists:seq(1,length(NodeList))),

    mnesia:wait_for_tables(lists:flatten(TableNamesList), 5000), % timeout is 5000ms for now

    % ?List all tables
    mnesia:system_info(tables),

    % ? Check if gn3_tiles is loaded and available
    mnesia:table_info(gn3_tiles, where_to_read),

    insert_map_to_database(Map),
    io:format("Initial map state loaded successfully to mnesia tables~n"),
    test_mnesia_tables(),
    ok.

%% Test all mnesia tables and verify data integrity
test_mnesia_tables() ->
    io:format("~n=== Testing Mnesia Tables ===~n"),
    
    % Test each GN table
    lists:foreach(fun(GN) ->
        test_gn_tables(GN)
    end, [1, 2, 3, 4]),
    
    
    io:format("=== Mnesia Table Tests Completed ===~n~n").

%% Test tables for specific Game Node
test_gn_tables(GN) ->
    TilesTable = generate_atom_table_names(GN, "_tiles"),
    BombsTable = generate_atom_table_names(GN, "_bombs"),
    PowerupsTable = generate_atom_table_names(GN, "_powerups"),
    PlayersTable = generate_atom_table_names(GN, "_players"),
    
    % Count tiles in this GN
    TileCount = mnesia:table_info(TilesTable, size),
    BombCount = mnesia:table_info(BombsTable, size),
    PowerupCount = mnesia:table_info(PowerupsTable, size),
    PlayerCount = mnesia:table_info(PlayersTable, size),
    
    io:format("GN~p - Tiles: ~p, Bombs: ~p, Powerups: ~p, Players: ~p~n", 
        [GN, TileCount, BombCount, PowerupCount, PlayerCount]).



%% --------------------------------------------------------------


%% helper function to create mnesia table names
generate_atom_table_names(Number, Type) ->
    list_to_atom("gn" ++ integer_to_list(Number) ++ Type).

create_tables(GN_node, CN_node, Node_number) ->
    Mnesia_tiles_name = generate_atom_table_names(Node_number, "_tiles"),
    Mnesia_bombs_name = generate_atom_table_names(Node_number, "_bombs"),
    Mnesia_powerups_name = generate_atom_table_names(Node_number, "_powerups"),
    Mnesia_players_name = generate_atom_table_names(Node_number, "_players"),
    %% initialize all mnesia tables, per game node
    mnesia:create_table(Mnesia_tiles_name, [
        {attributes, record_info(fields, mnesia_tiles)},
        {disc_copies, [CN_node]},
        {ram_copies, [GN_node]},
        {record_name, mnesia_tiles},
        {type, set}
        ]),
    mnesia:create_table(Mnesia_bombs_name, [
        {attributes, record_info(fields, mnesia_bombs)},
        {disc_copies, [CN_node]},
        {ram_copies, [GN_node]},
        {record_name, mnesia_bombs},
        {type, set}
    ]),
    mnesia:create_table(Mnesia_powerups_name, [
        {attributes, record_info(fields, mnesia_powerups)},
        {disc_copies, [CN_node]},
        {ram_copies, [GN_node]},
        {record_name, mnesia_powerups},
        {type, set}
    ]),
    mnesia:create_table(Mnesia_players_name, [
        {attributes, record_info(fields, mnesia_players)},
        {disc_copies, [CN_node]},
        {ram_copies, [GN_node]},
        {record_name, mnesia_players},
        {type, set}
    ]),
    io:format("CN: Initialized the following tables: ~w , ~w , ~w , ~w successfully~n",
        [Mnesia_tiles_name,Mnesia_bombs_name, Mnesia_powerups_name, Mnesia_players_name]),
    [Mnesia_tiles_name, Mnesia_bombs_name, Mnesia_powerups_name, Mnesia_players_name].


%% ============ Helper functions - inserting to table ============
-define (empty_tile, free).
-define (breakable_tile, breakable).
-define (two_hit_tile, strong).
-define (one_hit_tile, one_hit).
-define (unbreakable_tile, unbreakable).

insert_map_to_database(Map) ->
    % full map size - 16x16 [0->15][0->15]
    lists:foreach(fun(X) ->
            lists:foreach(fun(Y) ->
                {TileType, PowerupType, _BombType, PlayerID} = get_tile_content(X,Y, Map),
                if
                    TileType == player_start -> % player at this location
                        init_player([X,Y], PlayerID);
                    TileType =/= free -> % tile "exists"
                        insert_tile([X,Y], TileType, PowerupType);
                    true -> ok % empty tiles aren't stored in database
                end
            end,
            lists:seq(0,15)) end,
        lists:seq(0,15)),
    ok.

%% lessen my suffering in getting content of [X,Y]
get_tile_content(Pos_x, Pos_y, Map) ->
    array:get(Pos_y, array:get(Pos_x, Map)).

%% inserts the tile to its appropriate position
insert_tile(Position=[X,Y], Type, Contains) ->
%% inserts tile to appropriate table - synchronously
    %% Map partitioning:
    %% __________
    %%| GN1| GN2|
    %% ---------
    %%| GN3| GN4|
    %%-----------
    F = fun() ->
        Inserted_record = #mnesia_tiles{position = Position, type = Type, contains = Contains},
        case true of
            _ when X >= 0, X =< 7 , Y > 7 , Y =< 15 -> % GN1
                mnesia:write(gn1_tiles, Inserted_record, write);
            _ when X > 7, X =< 15 , Y > 7 , Y =< 15 -> % GN2
                mnesia:write(gn2_tiles, Inserted_record, write);
            _ when X >= 0 , X =< 7 , Y >= 0 , Y =< 7 -> % GN3
                mnesia:write(gn3_tiles, Inserted_record, write);
            _ when X > 7 , X =< 15 , Y >= 0 , Y =< 7 -> % GN4
                mnesia:write(gn4_tiles, Inserted_record, write)
        end end,
        mnesia:activity(transaction, F).

%% Initialize a player in the appropriate mnesia player table
init_player([X,Y], PlayerID) ->
    Fun = fun() ->
        Init_player_record = #mnesia_players{
            player_ID = PlayerID,
            position = [X,Y],
            next_position = none
        },
        case PlayerID of
            'player_1' ->
                mnesia:write(gn1_players, Init_player_record, write);
            'player_2' ->
                mnesia:write(gn2_players, Init_player_record, write);
            'player_3' ->
                mnesia:write(gn3_players, Init_player_record, write);
            'player_4' ->
                mnesia:write(gn4_players, Init_player_record, write)
        end end,
    io:format("CN: initialized a player entry ~w at location ~w~n",[PlayerID, [X,Y]]),
    mnesia:activity(transaction, Fun).

