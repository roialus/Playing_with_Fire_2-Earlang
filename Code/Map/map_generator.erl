-module(map_generator).

-export([generate_map/0, generate_map/1, generate_map_with_powerups/0, generate_map_with_powerups/1,
         visualize_map/1, get_tile_at/3, export_map/2, export_map_with_powerups/2, test_generation/0]).

%% Tile type definitions
-define(FREE, 0).         % Free spot (no tile) - walkable
-define(BREAKABLE, 1).    % Breakable tile - destructible in 1 explosion
-define(UNBREAKABLE, 2).  % Unbreakable wall - permanent obstacle
-define(STRONG, 3).       % Strong tile - needs 2 explosions to break
-define(PLAYER_START, 4). % Player starting position

%% Power-up definitions
-define(NO_POWERUP, none).
-define(MOVE_SPEED, move_speed).
-define(REMOTE_IGNITION, remote_ignition).
-define(REPEAT_BOMBS, repeat_bombs).
-define(KICK_BOMB, kick_bomb).
-define(PHASED, phased).
-define(PLUS_BOMBS, plus_bombs).
-define(BIGGER_EXPLOSION, bigger_explosion).
-define(PLUS_LIFE, plus_life).
-define(FREEZE_BOMB, freeze_bomb).

-define(MAP_SIZE, 16).

-record(map_state, {
    size = ?MAP_SIZE,
    grid,
    powerup_grid,  % Stores power-up information for each tile
    corners = [{1, 1}, {1, 14}, {14, 1}, {14, 14}],
    steiner_paths = [],
    statistics = #{} % Tracks tile and power-up counts
}).

%% Power-up configuration for random selection
-define(POWERUP_CONFIG, [
    % Rare power-ups (1 number each)
    {?REMOTE_IGNITION, 1, rare},
    {?REPEAT_BOMBS, 2, rare},
    {?PHASED, 3, rare},
    {?FREEZE_BOMB, 4, rare},
    % Regular power-ups (2 numbers each)
    {?MOVE_SPEED, [5, 6], regular},
    {?KICK_BOMB, [7, 8], regular},
    {?PLUS_BOMBS, [9, 10], regular},
    {?BIGGER_EXPLOSION, [11, 12], regular},
    {?PLUS_LIFE, [13, 14], regular}
]).

%% ===================================================================
%% Public API - Simple Map Generation
%% ===================================================================

generate_map() ->
    generate_map(#{}).

generate_map(Options) ->
    % Default options for map tiles
    DefaultOptions = #{
        steiner_breakable_chance => 0.3,
        breakable_prob => 0.4,
        strong_prob => 0.15,
        unbreakable_prob => 0.15,
        num_random_terminals => 2
    },
    Opts = maps:merge(DefaultOptions, Options),
    
    % For debugging
    io:format("🗺️  Generating ~wx~w Erlang Map with Power-ups...~n", [?MAP_SIZE, ?MAP_SIZE]),
    
    % Step 1: Initialize empty map
    MapState = initialize_map(),
    
    % Step 2: Create graph for Steiner tree
    Graph = create_game_graph(),
    
    % Step 3: Select terminals (corners + random)
    Terminals = select_terminals(maps:get(num_random_terminals, Opts)),
    % For debugging
    io:format("📍 Terminals: ~p~n", [Terminals]),
    
    % Step 4: Generate Steiner tree (simplified MST approach)
    SteinerEdges = generate_steiner_tree(Graph, Terminals),
    SteinerPaths = extract_steiner_paths(SteinerEdges),
    % For debugging
    io:format("🌲 Steiner tree: ~w positions~n", [length(SteinerPaths)]),
    
    % Step 5: Apply Steiner tree to map
    MapState2 = apply_steiner_tree(MapState, SteinerPaths, maps:get(steiner_breakable_chance, Opts)),
    
    % Step 6: Fill remaining areas
    MapState3 = fill_remaining_areas(MapState2, Opts),
    
    % Step 7: Mark player starts
    FinalMapState = mark_player_starts(MapState3),
    
    % Step 8: Print statistics
    print_statistics(FinalMapState),
    
    % For debugging
    io:format("✅ Map generation complete!~n"),
    FinalMapState#map_state.grid.

%% Generate map and return full map state (including power-ups)
generate_map_with_powerups() ->
    generate_map_with_powerups(#{}).

generate_map_with_powerups(Options) ->
    % Same as generate_map but returns full MapState
    DefaultOptions = #{
        steiner_breakable_chance => 0.3,
        breakable_prob => 0.4,
        strong_prob => 0.15,
        unbreakable_prob => 0.15,
        num_random_terminals => 2
    },
    Opts = maps:merge(DefaultOptions, Options),
    
    % For debugging
    io:format("🗺️  Generating ~wx~w Erlang Map with Power-ups...~n", [?MAP_SIZE, ?MAP_SIZE]),
    
    % Step 1: Initialize empty map
    MapState = initialize_map(),
    
    % Step 2: Create graph for Steiner tree
    Graph = create_game_graph(),
    
    % Step 3: Select terminals (corners + random)
    Terminals = select_terminals(maps:get(num_random_terminals, Opts)),
    % For debugging
    io:format("📍 Terminals: ~p~n", [Terminals]),
    
    % Step 4: Generate Steiner tree (simplified MST approach)
    SteinerEdges = generate_steiner_tree(Graph, Terminals),
    SteinerPaths = extract_steiner_paths(SteinerEdges),
    % For debugging
    io:format("🌲 Steiner tree: ~w positions~n", [length(SteinerPaths)]),
    
    % Step 5: Apply Steiner tree to map
    MapState2 = apply_steiner_tree(MapState, SteinerPaths, maps:get(steiner_breakable_chance, Opts)),
    
    % Step 6: Fill remaining areas
    MapState3 = fill_remaining_areas(MapState2, Opts),
    
    % Step 7: Mark player starts
    FinalMapState = mark_player_starts(MapState3),
    
    % Step 8: Print statistics
    print_statistics(FinalMapState),
    
    % For debugging
    io:format("✅ Map generation complete!~n"),
    FinalMapState.

%% ===================================================================
%% Map Initialization
%% ===================================================================

initialize_map() ->
    % Create 16x16 grid filled with FREE spots
    Grid = array:new(?MAP_SIZE, {default, ?FREE}),
    EmptyGrid = array:map(fun(_, _) -> 
        array:new(?MAP_SIZE, {default, ?FREE}) 
    end, Grid),
    
    % Create power-up grid (same structure)
    PowerupGrid = array:map(fun(_, _) -> 
        array:new(?MAP_SIZE, {default, ?NO_POWERUP}) 
    end, Grid),
    
    % Set borders to UNBREAKABLE
    BorderGrid = set_borders(EmptyGrid),
    
    % Clear player areas
    ClearGrid = clear_player_areas(BorderGrid),
    
    % Initialize statistics
    Stats = #{
        tile_counts => #{
            free => 0, breakable => 0, unbreakable => 0, 
            strong => 0, player_start => 0
        },
        powerup_counts => #{
            none => 0, move_speed => 0, remote_ignition => 0,
            repeat_bombs => 0, kick_bomb => 0, phased => 0,
            plus_bombs => 0, bigger_explosion => 0, plus_life => 0,
            freeze_bomb => 0
        },
        powerup_locations => []
    },
    
    #map_state{grid = ClearGrid, powerup_grid = PowerupGrid, statistics = Stats}.

set_borders(Grid) ->
    % Set top and bottom borders
    TopRow = array:new(?MAP_SIZE, {default, ?UNBREAKABLE}), 
    BottomRow = array:new(?MAP_SIZE, {default, ?UNBREAKABLE}),
    
    Grid1 = array:set(0, TopRow, Grid),
    Grid2 = array:set(?MAP_SIZE - 1, BottomRow, Grid1),
    
    % Set left and right borders
    array:map(fun(X, Row) when X > 0, X < ?MAP_SIZE - 1 ->
        Row1 = array:set(0, ?UNBREAKABLE, Row), 
        array:set(?MAP_SIZE - 1, ?UNBREAKABLE, Row1);
    (_, Row) -> Row
    end, Grid2).

clear_player_areas(Grid) ->
    Corners = [{1, 1}, {1, 14}, {14, 1}, {14, 14}],
    
    lists:foldl(fun({CX, CY}, AccGrid) ->
        % Clear 2x2 area around each corner
        lists:foldl(fun(DX, Grid1) ->
            lists:foldl(fun(DY, Grid2) ->
                X = CX + DX,    
                Y = CY + DY,
                case is_valid_position(X, Y) of
                    true ->
                        Row = array:get(X, Grid2),
                        NewRow = array:set(Y, ?FREE, Row),
                        array:set(X, NewRow, Grid2);
                    false -> Grid2
                end
            end, Grid1, [-1, 0, 1])
        end, AccGrid, [-1, 0, 1])
    end, Grid, Corners).

is_valid_position(X, Y) ->
    X >= 1 andalso X < ?MAP_SIZE - 1 andalso Y >= 1 andalso Y < ?MAP_SIZE - 1.

%% ===================================================================
%% Power-up Selection Logic
%% ===================================================================

select_powerup_for_breakable() ->
    RandNum = rand:uniform(25), % 1-25
    select_powerup_by_number(RandNum, breakable).

select_powerup_for_strong() ->
    RandNum = rand:uniform(14), % 1-14 (guaranteed power-up)
    select_powerup_by_number(RandNum, strong).

select_powerup_by_number(Num, _TileType) ->
    Config = ?POWERUP_CONFIG,
    case find_powerup_by_number(Num, Config) of
        {found, Powerup} -> Powerup;
        not_found -> ?NO_POWERUP
    end.

find_powerup_by_number(_Num, []) -> not_found;
find_powerup_by_number(Num, [{Powerup, Numbers, _Rarity} | Rest]) ->
    case is_number_in_range(Num, Numbers) of
        true -> {found, Powerup};
        false -> find_powerup_by_number(Num, Rest)
    end.

is_number_in_range(Num, Num) when is_integer(Num) -> true; % Single number
is_number_in_range(Num, Numbers) when is_list(Numbers) ->
    lists:member(Num, Numbers);
is_number_in_range(_, _) -> false.

%% ===================================================================
%% Enhanced Tile Setting with Power-ups and Statistics
%% ===================================================================

set_tile_with_powerup(MapState, X, Y, TileType) ->
    Grid = MapState#map_state.grid,
    PowerupGrid = MapState#map_state.powerup_grid,
    Stats = MapState#map_state.statistics,
    
    % Determine power-up based on tile type
    Powerup = case TileType of
        ?BREAKABLE -> select_powerup_for_breakable();
        ?STRONG -> select_powerup_for_strong();
        _ -> ?NO_POWERUP
    end,
    
    % Update grids
    Row = array:get(X, Grid),
    NewRow = array:set(Y, TileType, Row),
    NewGrid = array:set(X, NewRow, Grid),
    
    PowerupRow = array:get(X, PowerupGrid),
    NewPowerupRow = array:set(Y, Powerup, PowerupRow),
    NewPowerupGrid = array:set(X, NewPowerupRow, PowerupGrid),
    
    % Update statistics
    NewStats = update_statistics(Stats, TileType, Powerup, X, Y),
    
    MapState#map_state{
        grid = NewGrid,
        powerup_grid = NewPowerupGrid,
        statistics = NewStats
    }.

update_statistics(Stats, TileType, Powerup, X, Y) ->
    % Update tile counts
    TileCounts = maps:get(tile_counts, Stats),
    TileKey = tile_type_to_key(TileType),
    NewTileCounts = maps:update_with(TileKey, fun(Count) -> Count + 1 end, 1, TileCounts),
    
    % Update power-up counts
    PowerupCounts = maps:get(powerup_counts, Stats),
    PowerupKey = powerup_to_key(Powerup),
    NewPowerupCounts = maps:update_with(PowerupKey, fun(Count) -> Count + 1 end, 1, PowerupCounts),
    
    % Update power-up locations
    PowerupLocations = maps:get(powerup_locations, Stats),
    NewPowerupLocations = case Powerup of
        ?NO_POWERUP -> PowerupLocations;
        _ -> [{Powerup, X, Y, TileType} | PowerupLocations]
    end,
    
    Stats#{
        tile_counts => NewTileCounts,
        powerup_counts => NewPowerupCounts,
        powerup_locations => NewPowerupLocations
    }.

tile_type_to_key(?FREE) -> free;
tile_type_to_key(?BREAKABLE) -> breakable;
tile_type_to_key(?UNBREAKABLE) -> unbreakable;
tile_type_to_key(?STRONG) -> strong;
tile_type_to_key(?PLAYER_START) -> player_start.

powerup_to_key(?NO_POWERUP) -> none;
powerup_to_key(?MOVE_SPEED) -> move_speed;
powerup_to_key(?REMOTE_IGNITION) -> remote_ignition;
powerup_to_key(?REPEAT_BOMBS) -> repeat_bombs;
powerup_to_key(?KICK_BOMB) -> kick_bomb;
powerup_to_key(?PHASED) -> phased;
powerup_to_key(?PLUS_BOMBS) -> plus_bombs;
powerup_to_key(?BIGGER_EXPLOSION) -> bigger_explosion;
powerup_to_key(?PLUS_LIFE) -> plus_life;
powerup_to_key(?FREEZE_BOMB) -> freeze_bomb.

%% ===================================================================
%% Graph Creation for Steiner Tree (unchanged)
%% ===================================================================

create_game_graph() ->
    % Create list of all valid positions (nodes)
    Nodes = [{X, Y} || X <- lists:seq(1, ?MAP_SIZE - 2), 
                       Y <- lists:seq(1, ?MAP_SIZE - 2)],
    
    % Create edges between adjacent nodes
    Edges = lists:flatten([
        [{{X, Y}, {X + DX, Y + DY}} || 
         {DX, DY} <- [{0, 1}, {1, 0}, {0, -1}, {-1, 0}],
         is_valid_position(X + DX, Y + DY)]
        || {X, Y} <- Nodes
    ]),
    
    #{nodes => Nodes, edges => Edges}.

%% ===================================================================
%% Terminal Selection (unchanged)
%% ===================================================================

select_terminals(NumRandom) ->
    Corners = [{1, 1}, {1, 14}, {14, 1}, {14, 14}],
    
    % Add random terminals
    RandomTerminals = select_random_terminals(NumRandom, Corners, []),
    
    Corners ++ RandomTerminals.

select_random_terminals(0, _Existing, Acc) -> Acc;
select_random_terminals(N, Existing, Acc) ->
    X = rand:uniform(?MAP_SIZE - 6) + 2, % Range 3 to MAP_SIZE-4
    Y = rand:uniform(?MAP_SIZE - 6) + 2,    
    
    case is_valid_terminal({X, Y}, Existing ++ Acc) of
        true -> select_random_terminals(N - 1, Existing, [{X, Y} | Acc]);
        false -> select_random_terminals(N, Existing, Acc)
    end.

is_valid_terminal({X, Y}, ExistingTerminals) ->
    % Check minimum distance of 3 from existing terminals
    lists:all(fun({TX, TY}) ->
        abs(X - TX) >= 3 orelse abs(Y - TY) >= 3
    end, ExistingTerminals).

%% ===================================================================
%% Simplified Steiner Tree Generation (unchanged)
%% ===================================================================

generate_steiner_tree(Graph, Terminals) ->
    % MST of terminals with shortest paths
    #{nodes := Nodes, edges := Edges} = Graph,
    
    % Calculate distances between all terminals
    TerminalDistances = calculate_terminal_distances(Terminals),
    
    % Generate MST of terminals
    MST = minimum_spanning_tree(Terminals, TerminalDistances),
    
    % Convert MST edges to actual paths
    lists:flatten([shortest_path(From, To) || {From, To} <- MST]).

calculate_terminal_distances(Terminals) ->
    [{{T1, T2}, manhattan_distance(T1, T2)} || 
     T1 <- Terminals, T2 <- Terminals, T1 < T2].

manhattan_distance({X1, Y1}, {X2, Y2}) ->
    abs(X1 - X2) + abs(Y1 - Y2).

minimum_spanning_tree(Terminals, Distances) ->
    % Kruskal algorithm for MST
    SortedEdges = lists:sort(fun({_, D1}, {_, D2}) -> D1 =< D2 end, Distances),
    
    % Union-Find for cycle detection
    UnionFind = initialize_union_find(Terminals),
    
    select_mst_edges(SortedEdges, UnionFind, []).

initialize_union_find(Terminals) ->
    maps:from_list([{T, T} || T <- Terminals]).

select_mst_edges([], _UF, Acc) -> Acc;
select_mst_edges([{{T1, T2}, _} | Rest], UF, Acc) ->
    Root1 = find_root(T1, UF),
    Root2 = find_root(T2, UF),
    
    case Root1 =:= Root2 of
        true -> 
            select_mst_edges(Rest, UF, Acc);
        false ->
            NewUF = maps:put(Root1, Root2, UF),
            select_mst_edges(Rest, NewUF, [{T1, T2} | Acc])
    end.

find_root(Node, UF) ->
    case maps:get(Node, UF) of
        Node -> Node;
        Parent -> find_root(Parent, UF)
    end.

shortest_path({X1, Y1}, {X2, Y2}) ->
    % Simple path generation (Manhattan distance)
    XPath = if 
        X1 < X2 -> [{X, Y1} || X <- lists:seq(X1, X2)];
        X1 > X2 -> [{X, Y1} || X <- lists:seq(X2, X1)];
        true -> [{X1, Y1}]
    end,
    
    YPath = if 
        Y1 < Y2 -> [{X2, Y} || Y <- lists:seq(Y1, Y2)];
        Y1 > Y2 -> [{X2, Y} || Y <- lists:seq(Y2, Y1)];
        true -> []
    end,
    
    lists:usort(XPath ++ YPath).

extract_steiner_paths(SteinerEdges) ->
    lists:usort(lists:flatten(SteinerEdges)).

%% ===================================================================
%% Apply Steiner Tree to Map (Enhanced with Power-ups)
%% ===================================================================

apply_steiner_tree(MapState, SteinerPaths, BreakableChance) ->
    lists:foldl(fun({X, Y}, AccMapState) ->
        case is_valid_position(X, Y) andalso not is_in_player_area(X, Y) of
            true ->
                % Steiner paths can be FREE or BREAKABLE
                TileType = case rand:uniform() =< BreakableChance of
                    true -> ?BREAKABLE;
                    false -> ?FREE
                end,
                set_tile_with_powerup(AccMapState, X, Y, TileType);
            false ->
                AccMapState
        end
    end, MapState#map_state{steiner_paths = SteinerPaths}, SteinerPaths).

is_in_player_area(X, Y) ->
    Corners = [{1, 1}, {1, 14}, {14, 1}, {14, 14}],
    lists:any(fun({CX, CY}) ->
        abs(X - CX) =< 1 andalso abs(Y - CY) =< 1
    end, Corners).

%% ===================================================================
%% Fill Remaining Areas (Enhanced with Power-ups)
%% ===================================================================

fill_remaining_areas(MapState, Options) ->
    SteinerPaths = MapState#map_state.steiner_paths,    
    
    BreakableProb = maps:get(breakable_prob, Options),
    StrongProb = maps:get(strong_prob, Options),
    UnbreakableProb = maps:get(unbreakable_prob, Options),
    
    % Find all non-Steiner, non-player area positions
    AvailablePositions = get_available_positions(SteinerPaths),
    
    % Fill with random tile types
    lists:foldl(fun({X, Y}, AccMapState) ->
        TileType = select_random_tile_type(BreakableProb, StrongProb, UnbreakableProb),
        set_tile_with_powerup(AccMapState, X, Y, TileType)
    end, MapState, AvailablePositions).

get_available_positions(SteinerPaths) ->
    AllPositions = [{X, Y} || X <- lists:seq(1, ?MAP_SIZE - 2), 
                             Y <- lists:seq(1, ?MAP_SIZE - 2)],
    
    lists:filter(fun({X, Y}) ->
        not lists:member({X, Y}, SteinerPaths) andalso 
        not is_in_player_area(X, Y)
    end, AllPositions).

select_random_tile_type(BreakableProb, StrongProb, UnbreakableProb) ->
    Rand = rand:uniform(),
    
    if 
        Rand =< UnbreakableProb -> ?UNBREAKABLE;
        Rand =< UnbreakableProb + StrongProb -> ?STRONG;
        Rand =< BreakableProb + StrongProb + UnbreakableProb -> ?BREAKABLE;
        true -> ?FREE
    end.

%% ===================================================================
%% Mark Player Starts (Enhanced with Statistics)
%% ===================================================================

mark_player_starts(MapState) ->
    Corners = [{1, 1}, {1, 14}, {14, 1}, {14, 14}],
    
    lists:foldl(fun({X, Y}, AccMapState) ->
        set_tile_with_powerup(AccMapState, X, Y, ?PLAYER_START)
    end, MapState, Corners).

%% ===================================================================
%% Statistics Display
%% ===================================================================

print_statistics(MapState) ->
    Stats = MapState#map_state.statistics,
    TileCounts = maps:get(tile_counts, Stats),
    PowerupCounts = maps:get(powerup_counts, Stats),
    PowerupLocations = maps:get(powerup_locations, Stats),
    
    io:format("~n📊 MAP GENERATION STATISTICS~n"),
    io:format("============================~n"),
    
    % Tile counts
    io:format("🧱 TILE COUNTS:~n"),
    maps:foreach(fun(TileType, Count) ->
        io:format("  ~15s: ~3w~n", [atom_to_list(TileType), Count])
    end, TileCounts),
    
    io:format("~n💎 POWER-UP COUNTS:~n"),
    maps:foreach(fun(PowerupType, Count) ->
        io:format("  ~15s: ~3w~n", [atom_to_list(PowerupType), Count])
    end, PowerupCounts),
    
    io:format("~n📍 POWER-UP LOCATIONS:~n"),
    SortedLocations = lists:sort(PowerupLocations),
    lists:foreach(fun({Powerup, X, Y, TileType}) ->
        TileStr = case TileType of
            ?BREAKABLE -> "BREAKABLE";
            ?STRONG -> "STRONG";
            _ -> "OTHER"
        end,
        io:format("  ~15s at (~2w,~2w) in ~s tile~n", 
                 [atom_to_list(Powerup), X, Y, TileStr])
    end, SortedLocations),
    
    io:format("~n").

%% ===================================================================
%% Utility Functions (Enhanced)
%% ===================================================================

%% Get the tile type at specific coordinates
get_tile_at(Grid, X, Y) ->
    case X >= 0 andalso X < ?MAP_SIZE andalso Y >= 0 andalso Y < ?MAP_SIZE of
        true ->
            Row = array:get(X, Grid),
            array:get(Y, Row);
        false ->
            ?UNBREAKABLE % Out of bounds
    end.

%% Get the power-up at specific coordinates
get_powerup_at(PowerupGrid, X, Y) ->
    case X >= 0 andalso X < ?MAP_SIZE andalso Y >= 0 andalso Y < ?MAP_SIZE of
        true ->
            Row = array:get(X, PowerupGrid),
            array:get(Y, Row);
        false ->
            ?NO_POWERUP % Out of bounds
    end.

%% Enhanced visualization with power-ups
visualize_map(Grid) ->
    io:format("~n🗺️  Generated Map (~wx~w):~n", [?MAP_SIZE, ?MAP_SIZE]),
    io:format("   "),
    [io:format("~2w", [Y]) || Y <- lists:seq(0, ?MAP_SIZE - 1)],
    io:format("~n"),
    
    lists:foreach(fun(X) ->
        io:format("~2w ", [X]),
        Row = array:get(X, Grid),
        lists:foreach(fun(Y) ->
            Tile = array:get(Y, Row),
            Char = case Tile of
                ?FREE -> " .";
                ?BREAKABLE -> " *";
                ?UNBREAKABLE -> " #";
                ?STRONG -> " +";     
                ?PLAYER_START -> " P"
            end,
            io:format("~s", [Char])
        end, lists:seq(0, ?MAP_SIZE - 1)),
        io:format("~n")
    end, lists:seq(0, ?MAP_SIZE - 1)),
    
    io:format("~nLegend: . = Free, * = Breakable, # = Unbreakable, + = Strong, P = Player~n").

%% ===================================================================
%% Export Map to Erlang Module (Enhanced)
%% ===================================================================
export_map(Grid, Filename) ->
    {ok, File} = file:open(Filename, [write]),
    
    % Write header
    io:format(File, "%% Generated 16x16 Bomberman Map with Power-ups~n", []),
    io:format(File, "-module(~s).~n", [filename:basename(Filename, ".erl")]),
    io:format(File, "-export([get_map/0, get_tile_type/2, get_player_starts/0]).~n~n", []),
    
    % Write tile definitions
    io:format(File, "-define(FREE, 0).~n", []),
    io:format(File, "-define(BREAKABLE, 1).~n", []),
    io:format(File, "-define(UNBREAKABLE, 2).~n", []),
    io:format(File, "-define(STRONG, 3).~n", []),
    io:format(File, "-define(PLAYER_START, 4).~n~n", []),
    
    % Write map data
    io:format(File, "get_map() ->~n    [~n", []),
    lists:foreach(fun(X) ->
        Row = array:get(X, Grid),
        RowList = [array:get(Y, Row) || Y <- lists:seq(0, ?MAP_SIZE - 1)],
        RowStr = string:join([integer_to_list(T) || T <- RowList], ", "),
        Comma = if X < ?MAP_SIZE - 1 -> ","; true -> "" end,
        io:format(File, "        [~s]~s~n", [RowStr, Comma])
    end, lists:seq(0, ?MAP_SIZE - 1)),
    io:format(File, "    ].~n~n", []),
    
    % Write helper functions
    io:format(File, "get_tile_type(X, Y) when X >= 0, X < 16, Y >= 0, Y < 16 ->~n", []),
    io:format(File, "    Map = get_map(),~n", []),
    io:format(File, "    Row = lists:nth(X + 1, Map),~n", []),
    io:format(File, "    lists:nth(Y + 1, Row).~n~n", []),
    
    io:format(File, "get_player_starts() ->~n", []),
    io:format(File, "    [{player_1, 1, 1}, {player_2, 1, 14}, {player_3, 14, 1}, {player_4, 14, 14}].~n", []),
    
    file:close(File),
    io:format("📄 Map exported to ~s~n", [Filename]).

%% Enhanced export with power-up information
export_map_with_powerups(MapState, Filename) ->
    {ok, File} = file:open(Filename, [write]),
    
    Grid = MapState#map_state.grid,
    PowerupGrid = MapState#map_state.powerup_grid,
    
    % Write header
    io:format(File, "%% Generated 16x16 Bomberman Map with Power-ups~n", []),
    io:format(File, "-module(~s).~n", [filename:basename(Filename, ".erl")]),
    io:format(File, "-export([get_map/0, get_powerup_map/0, get_tile_type/2, get_powerup_at/2, get_player_starts/0]).~n~n", []),
    
    % Write tile definitions
    io:format(File, "-define(FREE, 0).~n", []),
    io:format(File, "-define(BREAKABLE, 1).~n", []),
    io:format(File, "-define(UNBREAKABLE, 2).~n", []),
    io:format(File, "-define(STRONG, 3).~n", []),
    io:format(File, "-define(PLAYER_START, 4).~n~n", []),
    
    % Write power-up definitions
    io:format(File, "-define(NO_POWERUP, none).~n", []),
    io:format(File, "-define(MOVE_SPEED, move_speed).~n", []),
    io:format(File, "-define(REMOTE_IGNITION, remote_ignition).~n", []),
    io:format(File, "-define(REPEAT_BOMBS, repeat_bombs).~n", []),
    io:format(File, "-define(KICK_BOMB, kick_bomb).~n", []),
    io:format(File, "-define(PHASED, phased).~n", []),
    io:format(File, "-define(PLUS_BOMBS, plus_bombs).~n", []),
    io:format(File, "-define(BIGGER_EXPLOSION, bigger_explosion).~n", []),
    io:format(File, "-define(PLUS_LIFE, plus_life).~n", []),
    io:format(File, "-define(FREEZE_BOMB, freeze_bomb).~n~n", []),
    
    % Write map data
    io:format(File, "get_map() ->~n    [~n", []),
    lists:foreach(fun(X) ->
        Row = array:get(X, Grid),
        RowList = [array:get(Y, Row) || Y <- lists:seq(0, ?MAP_SIZE - 1)],
        RowStr = string:join([integer_to_list(T) || T <- RowList], ", "),
        Comma = if X < ?MAP_SIZE - 1 -> ","; true -> "" end,
        io:format(File, "        [~s]~s~n", [RowStr, Comma])
    end, lists:seq(0, ?MAP_SIZE - 1)),
    io:format(File, "    ].~n~n", []),
    
    % Write power-up map data
    io:format(File, "get_powerup_map() ->~n    [~n", []),
    lists:foreach(fun(X) ->
        Row = array:get(X, PowerupGrid),
        RowList = [array:get(Y, Row) || Y <- lists:seq(0, ?MAP_SIZE - 1)],
        RowStr = string:join([format_powerup_for_export(P) || P <- RowList], ", "),
        Comma = if X < ?MAP_SIZE - 1 -> ","; true -> "" end,
        io:format(File, "        [~s]~s~n", [RowStr, Comma])
    end, lists:seq(0, ?MAP_SIZE - 1)),
    io:format(File, "    ].~n~n", []),
    
    % Write helper functions
    io:format(File, "get_tile_type(X, Y) when X >= 0, X < 16, Y >= 0, Y < 16 ->~n", []),
    io:format(File, "    Map = get_map(),~n", []),
    io:format(File, "    Row = lists:nth(X + 1, Map),~n", []),
    io:format(File, "    lists:nth(Y + 1, Row).~n~n", []),
    
    io:format(File, "get_powerup_at(X, Y) when X >= 0, X < 16, Y >= 0, Y < 16 ->~n", []),
    io:format(File, "    PowerupMap = get_powerup_map(),~n", []),
    io:format(File, "    Row = lists:nth(X + 1, PowerupMap),~n", []),
    io:format(File, "    lists:nth(Y + 1, Row).~n~n", []),
    
    io:format(File, "get_player_starts() ->~n", []),
    io:format(File, "    [{player_1, 1, 1}, {player_2, 1, 14}, {player_3, 14, 1}, {player_4, 14, 14}].~n", []),
    
    file:close(File),
    io:format("📄 Map with power-ups exported to ~s~n", [Filename]).

%% Helper function to format power-ups for export
format_powerup_for_export(Powerup) ->
    case Powerup of
        ?NO_POWERUP -> "none";
        ?MOVE_SPEED -> "move_speed";
        ?REMOTE_IGNITION -> "remote_ignition";
        ?REPEAT_BOMBS -> "repeat_bombs";
        ?KICK_BOMB -> "kick_bomb";
        ?PHASED -> "phased";
        ?PLUS_BOMBS -> "plus_bombs";
        ?BIGGER_EXPLOSION -> "bigger_explosion";
        ?PLUS_LIFE -> "plus_life";
        ?FREEZE_BOMB -> "freeze_bomb"
    end.

%% ===================================================================
%% Test Functions
%% ===================================================================

test_generation() ->
    io:format("🧪 Testing map generation with power-ups...~n"),
    
    % Generate map with full state
    MapState = generate_map_with_powerups(),
    Grid = MapState#map_state.grid,
    
    % Test basic properties
    BorderTile = get_tile_at(Grid, 0, 0),
    CenterTile = get_tile_at(Grid, 8, 8),
    PlayerTile = get_tile_at(Grid, 1, 1),
    
    io:format("Border tile: ~w (should be ~w)~n", [BorderTile, ?UNBREAKABLE]),
    io:format("Player start: ~w (should be ~w)~n", [PlayerTile, ?PLAYER_START]),
    io:format("Center area: ~w~n", [CenterTile]),
    
    % Visualize
    visualize_map(Grid),
    
    % Export both versions for testing
    export_map(Grid, "test_map_basic.erl"),
    export_map_with_powerups(MapState, "test_map_with_powerups.erl"),
    
    io:format("✅ Test complete!~n"),
    Grid.
