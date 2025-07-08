%%%-------------------------------------------------------------------
%%% @author dolev
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% Modified CN_test to work with test_unified_map
%%% @end
%%% Created : 06. Jul 2025 12:18
%%%-------------------------------------------------------------------
-module('cn_test_new').
-author("dolev").

%% API
-export([test/1, test_with_map/2, test_mnesia_tables/0, print_all_tables/0]).

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

%% Test function that gets map from test_unified_map module
test(NodeList) ->
    %% NodeList = [node(), gn_node1, gn_node2, gn_node3, gn_node4] -- THIS IS HOW THIS LIST SHOULD LOOK LIKE
    io:format("Starting CN test...~n"), % For debugging purposes
    
    application:set_env(mnesia, dir, "~/Documents/mnesia_files"),
    mnesia:create_schema(NodeList),
    rpc:multicall(NodeList, application, start, [mnesia]), % multiple nodes
    %application:start(mnesia), % single node

    %% initialize mnesia tables per each game-node
    TableNamesList = lists:map(fun(X) ->
            create_tables(lists:nth(X, NodeList), node(), X)
        end, lists:seq(1,4)),
    mnesia:wait_for_tables(lists:flatten(TableNamesList), 5000), % timeout is 5000ms for now
    
    io:format("Tables created successfully. Inserting map data...~n"),  % For debugging purposes
    insert_map_to_database_unified(),
    
    io:format("Map data inserted. Running tests...~n"), % For debugging purposes
    test_mnesia_tables(),
    
    io:format("CN test completed successfully!~n"), % For debugging purposes
    ok.

%% Test function with provided map 
test_with_map(NodeList, Map) ->
    %% NodeList = [node(), gn_node1, gn_node2, gn_node3, gn_node4] -- THIS IS HOW THIS LIST SHOULD LOOK LIKE
    io:format("Starting CN test with provided map...~n"),   % For debugging purposes
    
    application:set_env(mnesia, dir, "~/Documents/mnesia_files"),
    mnesia:create_schema(NodeList),
    rpc:multicall(NodeList, application, start, [mnesia]), % multiple nodes

    %% initialize mnesia tables per each game-node
    TableNamesList = lists:map(fun(X) ->
            create_tables(lists:nth(X, NodeList), node(), X)
        end, lists:seq(1,4)),
    mnesia:wait_for_tables(lists:flatten(TableNamesList), 5000), % timeout is 5000ms for now
    
    io:format("Tables created successfully. Inserting map data...~n"),  % For debugging purposes
    insert_map_to_database(Map),
    
    io:format("Map data inserted. Running tests...~n"), % For debugging purposes
    test_mnesia_tables(),
    
    io:format("CN test with provided map completed successfully!~n"),   % For debugging purposes
    ok.

%% helper function to create mnesia table names
generate_atom_table_names(Number, Type) ->
    list_to_atom("gn" ++ integer_to_list(Number) ++ Type).

create_tables(GN_node, CN_node, Node_number) ->
    Mnesia_tiles_name = generate_atom_table_names(Node_number, "_tiles"),
    Mnesia_bombs_name = generate_atom_table_names(Node_number, "_bombs"),
    Mnesia_powerups_name = generate_atom_table_names(Node_number, "_powerups"),
    Mnesia_players_name = generate_atom_table_names(Node_number, "_players"),
    
    io:format("Creating tables for GN~p: ~p, ~p, ~p, ~p~n", 
        [Node_number, Mnesia_tiles_name, Mnesia_bombs_name, Mnesia_powerups_name, Mnesia_players_name]),
    
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

%% Insert map using test_unified_map module
insert_map_to_database_unified() ->
    % full map size - 16x16 [0->15][0->15]
    TileCount = lists:foldl(fun(X, AccX) ->
            lists:foldl(fun(Y, AccY) ->
                {TileType, PowerupType, _BombType, PlayerID} = test_unified_map:get_cell_at(X, Y),
                if
                    TileType =/= free -> % tile "exists"
                        insert_tile([X,Y], TileType, PowerupType),
                        AccY + 1;
                    true -> AccY % empty tiles aren't stored in database
                end,
                if
                    PlayerID =/= none -> % there's a player at this location
                        ok; % TODO: PLACEHOLDER
                    true -> ok
                end,
                AccY
            end, AccX, lists:seq(0,15))
        end, 0, lists:seq(0,15)),
    io:format("Inserted ~p tiles into database~n", [TileCount]),
    ok.

%% Original insert map function
insert_map_to_database(Map) ->
    % full map size - 16x16 [0->15][0->15]
    TileCount = lists:foldl(fun(X, AccX) ->
            lists:foldl(fun(Y, AccY) ->
                {TileType, PowerupType, _BombType, PlayerID} = get_tile_content(X,Y, Map),
                if
                    TileType =/= free -> % tile "exists"
                        insert_tile([X,Y], TileType, PowerupType),
                        AccY + 1;
                    true -> AccY % empty tiles aren't stored in database
                end,
                if
                    PlayerID =/= none -> % there's a player at this location
                        ok; % TODO: PLACEHOLDER
                    true -> ok
                end,
                AccY
            end, AccX, lists:seq(0,15))
        end, 0, lists:seq(0,15)),
    io:format("Inserted ~p tiles into database~n", [TileCount]),
    ok.

%% Get tile content using test_unified_map
get_tile_content(Pos_x, Pos_y, _Map) ->
    % Use test_unified_map functions
    test_unified_map:get_cell_at(Pos_x, Pos_y).

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


%% ============ Testing Functions ============

%% Test all mnesia tables and verify data integrity
test_mnesia_tables() ->
    io:format("~n=== Testing Mnesia Tables ===~n"),
    
    % Test each GN table
    lists:foreach(fun(GN) ->
        test_gn_tables(GN)
    end, [1, 2, 3, 4]),
    
    % Test data distribution
    test_data_distribution(),
    
    % Test specific positions
    test_specific_positions(),
    
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

%% Test data distribution across game nodes
test_data_distribution() ->
    io:format("~nTesting data distribution:~n"),
    
    % Check that player starts are in correct positions
    PlayerStarts = test_unified_map:get_player_starts(),
    lists:foreach(fun({PlayerID, X, Y}) ->
        GN = determine_gn_for_position([X, Y]),
        PlayersTable = generate_atom_table_names(GN, "_players"),
        
        F = fun() ->
            mnesia:match_object(PlayersTable, #mnesia_players{name = PlayerID, _ = '_'}, read)
        end,
        
        % NOT RELEVANT FOR NOW
        case mnesia:activity(transaction, F) of
            [Player] ->
                io:format("Pass, ~p found in GN~p at position ~p~n", [PlayerID, GN, Player#mnesia_players.other]);
            [] ->
                io:format("Fail, ~p NOT found in any table~n", [PlayerID]);
            Multiple ->
                io:format("! ~p found in multiple locations: ~p~n", [PlayerID, length(Multiple)])
        end
    end, PlayerStarts).

%% Test specific known positions
test_specific_positions() ->
    io:format("~nTesting specific positions:~n"),
    
    % Test some known positions from the map
    TestPositions = [
        {0, 0, unbreakable, none},    % Corner
        {1, 1, player_start, none},   % Player 1 start
        {4, 1, breakable, plus_bombs}, % Breakable with powerup
        {3, 2, strong, phased}        % Strong tile with powerup
    ],
    
    lists:foreach(fun({X, Y, ExpectedType, ExpectedPowerup}) ->
        GN = determine_gn_for_position([X, Y]),
        TilesTable = generate_atom_table_names(GN, "_tiles"),
        
        F = fun() ->
            mnesia:read(TilesTable, [X, Y])
        end,
        
        case mnesia:activity(transaction, F) of
            [Tile] when Tile#mnesia_tiles.type =:= ExpectedType,
                        Tile#mnesia_tiles.contains =:= ExpectedPowerup ->
                io:format("Pass, Position [~p,~p] correct: ~p with ~p~n", 
                    [X, Y, ExpectedType, ExpectedPowerup]);
            [Tile] ->
                io:format("Fail, Position [~p,~p] mismatch: got ~p with ~p, expected ~p with ~p~n", 
                    [X, Y, Tile#mnesia_tiles.type, Tile#mnesia_tiles.contains, 
                     ExpectedType, ExpectedPowerup]);
            [] ->
                case ExpectedType of
                    free ->
                        io:format("Pass, Position [~p,~p] correctly empty (free tile)~n", [X, Y]);
                    _ ->
                        io:format("Fail, Position [~p,~p] not found but should contain ~p~n", 
                            [X, Y, ExpectedType])
                end;
            Multiple ->
                io:format("! Position [~p,~p] has multiple entries: ~p~n", [X, Y, length(Multiple)])
        end
    end, TestPositions).

%% Determine which GN a position belongs to
determine_gn_for_position([X, Y]) ->
    case true of
        _ when X >= 0, X =< 7 , Y > 7 , Y =< 15 -> 1; % GN1
        _ when X > 7, X =< 15 , Y > 7 , Y =< 15 -> 2; % GN2
        _ when X >= 0 , X =< 7 , Y >= 0 , Y =< 7 -> 3; % GN3
        _ when X > 7 , X =< 15 , Y >= 0 , Y =< 7 -> 4  % GN4
    end.

%% Print all tables content for debugging
print_all_tables() ->
    io:format("~n=== Printing All Table Contents ===~n"),
    
    lists:foreach(fun(GN) ->
        io:format("~n--- GN~p Tables ---~n", [GN]),
        print_gn_table_contents(GN)
    end, [1, 2, 3, 4]).

%% Print contents of tables for specific GN
print_gn_table_contents(GN) ->
    TilesTable = generate_atom_table_names(GN, "_tiles"),
    PlayersTable = generate_atom_table_names(GN, "_players"),
    
    % Print tiles
    io:format("Tiles in ~p:~n", [TilesTable]),
    F1 = fun() ->
        mnesia:foldl(fun(Tile, Acc) ->
            io:format("  ~p: ~p (~p)~n", 
                [Tile#mnesia_tiles.position, Tile#mnesia_tiles.type, Tile#mnesia_tiles.contains]),
            Acc + 1
        end, 0, TilesTable)
    end,
    TileCount = mnesia:activity(transaction, F1),
    io:format("Total tiles: ~p~n", [TileCount]),
    
    % Print players
    io:format("Players in ~p:~n", [PlayersTable]),
    F2 = fun() ->
        mnesia:foldl(fun(Player, Acc) ->
            io:format("  ~p at ~p (~p)~n", 
                [Player#mnesia_players.name, Player#mnesia_players.other, Player#mnesia_players.stats]),
            Acc + 1
        end, 0, PlayersTable)
    end,
    PlayerCount = mnesia:activity(transaction, F2),
    io:format("Total players: ~p~n", [PlayerCount]).