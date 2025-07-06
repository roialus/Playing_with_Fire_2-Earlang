%%%-------------------------------------------------------------------
%%% @author dolev
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Jul 2025 12:18
%%%-------------------------------------------------------------------
-module('CN_test').
-author("dolev").

%% API
-export([test/2]).

%% **NOTE:** when terminating, need to use application:stop(mnesia)

-record(mnesia_tiles, {
    position, % position - [X,Y]
    type, % can be - unbreakable, breakable, two_hit (-> one_hit)
    contains  % can be - none (no power-up), bomb (that'll trigger on its own), or any speed-up
}).

-record(mnesia_bombs, {
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

-record(mnesia_powerups, {
    position, % position - [X,Y]
    type, % type of power up - can be movement speed, extra bombs etc..
    original_node_ID % original creating node ID - TODO: unsure of necessity
}).

-record(mnesia_players, {
    name, % placeholder
    other,
    stats
}).

%% --------------------------------------------------------------

test(NodeList, Map) ->
    %% NodeList = [node(), gn_node1, gn_node2, gn_node3, gn_node4] -- THIS IS HOW THIS LIST SHOULD LOOK LIKE
    application:set_env(mnesia, dir, "~/Documents/mnesia_files"),
    mnesia:create_schema([NodeList]),
    rpc:multicall(NodeList, application, start, [mnesia]), % multiple nodes
    %application:start(mnesia), % single node

    %% initialize mnesia tables per each game-node
    TableNamesList = lists:map(fun(X) ->
            create_tables(lists:nth(X, NodeList), node(), X)
        end, lists:seq(1,4)),
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


