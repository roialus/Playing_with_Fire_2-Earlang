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
-export([test/1, connecting_nodes/1]).

%% **NOTE:** when terminating, need to use application:stop(mnesia)

-include("mnesia_records.hrl").

%% --------------------------------------------------------------
%% todo: for super-massive debugging use sys:trace(Pid, true).
%% @doc connecting nodes according to an IP list
connecting_nodes(IPList) ->
    %% each erl shell will be called 'GN#@IP where IP is from the list.
    IPsAsAtoms = lists:foreach(
        fun(X) -> list_to_atom("GN" ++ integer_to_list(X) ++"@" ++ lists:nth(X, IPList)) end,
        lists:seq(1,length(IPList))),
    lists:map(
        fun(IP) -> io:format("Pinging ~w : ~w~n",[IP, net_adm:ping(IP)]) end, IPsAsAtoms),
    IPsAsAtoms.

%% --------------------------------------------------------------

test(NodeList) ->
    %% NodeList = [node(), gn_node1, gn_node2, gn_node3, gn_node4] -- THIS IS HOW THIS LIST SHOULD LOOK LIKE
    Map = test_unified_map:get_map(),
    application:set_env(mnesia, dir, "~/Documents/mnesia_files"),
    mnesia:create_schema([NodeList]),
    rpc:multicall(NodeList, application, start, [mnesia]), % multiple nodes
    % application:start(mnesia), % single node

    %% initialize mnesia tables per each game-node
    TableNamesList = lists:map(fun(X) ->
            create_tables(lists:nth(X, NodeList), node(), X)
        end, lists:seq(1,length(NodeList))),
    mnesia:wait_for_tables(lists:flatten(TableNamesList), 5000), % timeout is 5000ms for now
    insert_map_to_database(Map),
    ok.


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
                    TileType =/= free -> % tile "exists"
                        insert_tile([X,Y], TileType, PowerupType);
                    true -> ok % empty tiles aren't stored in database
                end,
                if
                    PlayerID =/= none -> % there's a player at this location
                        ok; % TODO: PLACEHOLDER
                    true -> ok
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


