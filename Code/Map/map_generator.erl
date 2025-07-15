-module(map_generator).

-export([generate_map/0, generate_map/1, generate_map_with_powerups/0, generate_map_with_powerups/1,
         visualize_map/1, get_cell_at/3, export_map/2, test_generation/0]).

%% Tile type definitions (now atoms)
-define(FREE, free).
-define(BREAKABLE, breakable).
-define(UNBREAKABLE, unbreakable).
-define(STRONG, strong).
-define(PLAYER_START, player_start).

%% Power-up definitions (atoms)
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

%% Bomb type definitions (atoms)
-define(NO_BOMB, none).
-define(NORMAL_BOMB, normal_bomb).
-define(REMOTE_BOMB, remote_bomb).
-define(FREEZE_BOMB_ITEM, freeze_bomb_item).

%% Player ID definitions (atoms)
-define(NO_PLAYER, none).
-define(PLAYER_1, player_1).
-define(PLAYER_2, player_2).
-define(PLAYER_3, player_3).
-define(PLAYER_4, player_4).

-define(MAP_SIZE, 16).

%% Grid cell structure: {TileType, PowerupType, BombType, PlayerID}
-define(EMPTY_CELL, {?FREE, ?NO_POWERUP, ?NO_BOMB, ?NO_PLAYER}).
-define(UNBREAKABLE_CELL, {?UNBREAKABLE, ?NO_POWERUP, ?NO_BOMB, ?NO_PLAYER}).

-record(map_state, {
    size = ?MAP_SIZE,
    grid,  % Single grid with tuples: {tile_type, powerup_type, bomb_type, player_id}
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
    % Create 16x16 grid filled with empty cells
    Grid = array:new(?MAP_SIZE, {default, ?EMPTY_CELL}),
    EmptyGrid = array:map(fun(_, _) -> 
        array:new(?MAP_SIZE, {default, ?EMPTY_CELL}) 
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
    
    #map_state{grid = ClearGrid, statistics = Stats}.

set_borders(Grid) ->
    % Set top and bottom borders
    TopRow = array:new(?MAP_SIZE, {default, ?UNBREAKABLE_CELL}), 
    BottomRow = array:new(?MAP_SIZE, {default, ?UNBREAKABLE_CELL}),
    
    Grid1 = array:set(0, TopRow, Grid),
    Grid2 = array:set(?MAP_SIZE - 1, BottomRow, Grid1),
    
    % Set left and right borders
    array:map(fun(X, Row) when X > 0, X < ?MAP_SIZE - 1 ->
        Row1 = array:set(0, ?UNBREAKABLE_CELL, Row), 
        array:set(?MAP_SIZE - 1, ?UNBREAKABLE_CELL, Row1);
    (_, Row) -> Row
    end, Grid2).

clear_player_areas(Grid) ->
    Corners = [{1, 1}, {14, 1}, {1, 14}, {14, 14}],
    
    lists:foldl(fun({CX, CY}, AccGrid) ->
        % Clear 2x2 area around each corner
        lists:foldl(fun(DX, Grid1) ->
            lists:foldl(fun(DY, Grid2) ->
                X = CX + DX,    
                Y = CY + DY,
                case is_valid_position(X, Y) of
                    true ->
                        Row = array:get(X, Grid2),
                        NewRow = array:set(Y, ?EMPTY_CELL, Row),
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
%% Enhanced Cell Setting with Unified Grid
%% ===================================================================

set_cell(MapState, X, Y, TileType) ->
    set_cell(MapState, X, Y, TileType, ?NO_POWERUP, ?NO_BOMB, ?NO_PLAYER).

set_cell(MapState, X, Y, TileType, PowerupType, BombType, PlayerID) ->
    Grid = MapState#map_state.grid,
    Stats = MapState#map_state.statistics,
    
    % Create new cell
    NewCell = {TileType, PowerupType, BombType, PlayerID},
    
    % Update grid
    Row = array:get(X, Grid),
    NewRow = array:set(Y, NewCell, Row),
    NewGrid = array:set(X, NewRow, Grid),
    
    % Update statistics
    NewStats = update_statistics(Stats, TileType, PowerupType, X, Y),
    
    MapState#map_state{
        grid = NewGrid,
        statistics = NewStats
    }.

set_cell_with_powerup(MapState, X, Y, TileType) ->
    % Determine power-up based on tile type
    Powerup = case TileType of
        ?BREAKABLE -> select_powerup_for_breakable();
        ?STRONG -> select_powerup_for_strong();
        _ -> ?NO_POWERUP
    end,
    
    set_cell(MapState, X, Y, TileType, Powerup, ?NO_BOMB, ?NO_PLAYER).

%% Helper functions to work with cells
get_tile_type({TileType, _, _, _}) -> TileType.
get_powerup_type({_, PowerupType, _, _}) -> PowerupType.
get_bomb_type({_, _, BombType, _}) -> BombType.
get_player_id({_, _, _, PlayerID}) -> PlayerID.

%% Update cell components
update_tile_type({_, PowerupType, BombType, PlayerID}, NewTileType) ->
    {NewTileType, PowerupType, BombType, PlayerID}.

update_powerup_type({TileType, _, BombType, PlayerID}, NewPowerupType) ->
    {TileType, NewPowerupType, BombType, PlayerID}.

update_bomb_type({TileType, PowerupType, _, PlayerID}, NewBombType) ->
    {TileType, PowerupType, NewBombType, PlayerID}.

update_player_id({TileType, PowerupType, BombType, _}, NewPlayerID) ->
    {TileType, PowerupType, BombType, NewPlayerID}.

update_statistics(Stats, TileType, Powerup, X, Y) ->
    % Update tile counts
    TileCounts = maps:get(tile_counts, Stats),
    NewTileCounts = maps:update_with(TileType, fun(Count) -> Count + 1 end, 1, TileCounts),
    
    % Update power-up counts
    PowerupCounts = maps:get(powerup_counts, Stats),
    NewPowerupCounts = maps:update_with(Powerup, fun(Count) -> Count + 1 end, 1, PowerupCounts),
    
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
%% Apply Steiner Tree to Map (Enhanced with unified grid)
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
                set_cell_with_powerup(AccMapState, X, Y, TileType);
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
%% Fill Remaining Areas (Enhanced with unified grid)
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
        set_cell_with_powerup(AccMapState, X, Y, TileType)
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
%% Mark Player Starts (Enhanced with unified grid)
%% ===================================================================

mark_player_starts(MapState) ->
    Corners = [{1, 1}, {14, 1}, {1, 14}, {14, 14}],
    PlayerIDs = [?PLAYER_1, ?PLAYER_2, ?PLAYER_3, ?PLAYER_4],
    
    lists:foldl(fun({{X, Y}, PlayerID}, AccMapState) ->
        set_cell(AccMapState, X, Y, ?PLAYER_START, ?NO_POWERUP, ?NO_BOMB, PlayerID)
    end, MapState, lists:zip(Corners, PlayerIDs)).

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
        TileStr = atom_to_list(TileType),
        io:format("  ~15s at (~2w,~2w) in ~s tile~n", 
                 [atom_to_list(Powerup), X, Y, TileStr])
    end, SortedLocations),
    
    io:format("~n").

%% ===================================================================
%% Utility Functions (Enhanced for unified grid)
%% ===================================================================

%% Get the complete cell at specific coordinates
get_cell_at(Grid, X, Y) ->
    case X >= 0 andalso X < ?MAP_SIZE andalso Y >= 0 andalso Y < ?MAP_SIZE of
        true ->
            Row = array:get(X, Grid),
            array:get(Y, Row);
        false ->
            ?UNBREAKABLE_CELL % Out of bounds
    end.

%% Get specific components from cell
get_tile_at(Grid, X, Y) ->
    Cell = get_cell_at(Grid, X, Y),
    get_tile_type(Cell).

get_powerup_at(Grid, X, Y) ->
    Cell = get_cell_at(Grid, X, Y),
    get_powerup_type(Cell).

get_bomb_at(Grid, X, Y) ->
    Cell = get_cell_at(Grid, X, Y),
    get_bomb_type(Cell).

get_player_at(Grid, X, Y) ->
    Cell = get_cell_at(Grid, X, Y),
    get_player_id(Cell).

%% Enhanced visualization with complete cell information
visualize_map(Grid) ->
    io:format("~n🗺️  Generated Map (~wx~w):~n", [?MAP_SIZE, ?MAP_SIZE]),
    io:format("   "),
    [io:format("~2w", [Y]) || Y <- lists:seq(0, ?MAP_SIZE - 1)],
    io:format("~n"),
    
    lists:foreach(fun(X) ->
        io:format("~2w ", [X]),
        Row = array:get(X, Grid),
        lists:foreach(fun(Y) ->
            Cell = array:get(Y, Row),
            {TileType, PowerupType, BombType, PlayerID} = Cell,
            
            % Primary display based on tile type, but show other info if present
            Char = case {TileType, BombType, PlayerID} of
                {_, ?NORMAL_BOMB, _} -> " B";        % Bomb takes priority
                {_, ?REMOTE_BOMB, _} -> " R";        % Remote bomb
                {_, ?FREEZE_BOMB_ITEM, _} -> " F";   % Freeze bomb
                {_, _, ?PLAYER_1} -> " 1";           % Player 1
                {_, _, ?PLAYER_2} -> " 2";           % Player 2
                {_, _, ?PLAYER_3} -> " 3";           % Player 3
                {_, _, ?PLAYER_4} -> " 4";           % Player 4
                {?FREE, _, _} -> case PowerupType of
                    ?NO_POWERUP -> " .";
                    _ -> " o"  % Show power-up on free space
                end;
                {?BREAKABLE, _, _} -> " *";
                {?UNBREAKABLE, _, _} -> " #";
                {?STRONG, _, _} -> " +";     
                {?PLAYER_START, _, _} -> " P"
            end,
            io:format("~s", [Char])
        end, lists:seq(0, ?MAP_SIZE - 1)),
        io:format("~n")
    end, lists:seq(0, ?MAP_SIZE - 1)),
    
    io:format("~nLegend:~n"),
    io:format("  . = Free, * = Breakable, # = Unbreakable, + = Strong, P = Player Start~n"),
    io:format("  o = Power-up, B = Bomb, R = Remote Bomb, F = Freeze Bomb~n"),
    io:format("  1-4 = Players~n").

%% ===================================================================
%% Port communication for python graphical interface
%% ===================================================================

%% Start the Python port for graphical interface
start_grid_port() ->
    Port = open_port({spawn, "python3 map_live_port.py"}, 
                     [binary, exit_status, {packet, 4}]),
    register(grid_port, Port),
    Port.

%% Send the grid to the Python port for visualization
send_grid(Grid) ->
    Bin = term_to_binary(convert_array_grid_to_list(Grid)),
    grid_port ! {self(), {command, Bin}}.

%% Convert the array grid to a list of lists for Python visualization
convert_array_grid_to_list(Grid) ->
    [ [ array:get(Y, array:get(X, Grid)) || Y <- lists:seq(0, ?MAP_SIZE - 1) ]
        || X <- lists:seq(0, ?MAP_SIZE - 1) ].


%% ===================================================================
%% Export Map to Erlang Module (Enhanced for unified grid)
%% ===================================================================

export_map(Grid, Filename) ->
    {ok, File} = file:open(Filename, [write]),
    
    % Write header
    io:format(File, "%% Generated 16x16 Bomberman Map with Unified Grid~n", []),
    io:format(File, "-module(~s).~n", [filename:basename(Filename, ".erl")]),
    io:format(File, "-export([get_map/0, get_cell_at/2, get_tile_at/2, get_powerup_at/2, get_bomb_at/2, get_player_at/2, get_player_starts/0]).~n~n", []),
    
    % Write atom definitions
    io:format(File, "%% Tile type atoms~n", []),
    io:format(File, "-define(FREE, free).~n", []),
    io:format(File, "-define(BREAKABLE, breakable).~n", []),
    io:format(File, "-define(UNBREAKABLE, unbreakable).~n", []),
    io:format(File, "-define(STRONG, strong).~n", []),
    io:format(File, "-define(PLAYER_START, player_start).~n~n", []),
    
    io:format(File, "%% Power-up atoms~n", []),
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
    
    io:format(File, "%% Bomb type atoms~n", []),
    io:format(File, "-define(NO_BOMB, none).~n", []),
    io:format(File, "-define(NORMAL_BOMB, normal_bomb).~n", []),
    io:format(File, "-define(REMOTE_BOMB, remote_bomb).~n", []),
    io:format(File, "-define(FREEZE_BOMB_ITEM, freeze_bomb_item).~n~n", []),
    
    io:format(File, "%% Player ID atoms~n", []),
    io:format(File, "-define(NO_PLAYER, none).~n", []),
    io:format(File, "-define(PLAYER_1, player_1).~n", []),
    io:format(File, "-define(PLAYER_2, player_2).~n", []),
    io:format(File, "-define(PLAYER_3, player_3).~n", []),
    io:format(File, "-define(PLAYER_4, player_4).~n~n", []),
    
    % Write map data (unified grid with tuples)
    io:format(File, "get_map() ->~n    [~n", []),
    lists:foreach(fun(X) ->
        Row = array:get(X, Grid),
        RowList = [array:get(Y, Row) || Y <- lists:seq(0, ?MAP_SIZE - 1)],
        RowStr = string:join([format_cell_for_export(Cell) || Cell <- RowList], ", "),
        Comma = if X < ?MAP_SIZE - 1 -> ","; true -> "" end,
        io:format(File, "        [~s]~s~n", [RowStr, Comma])
    end, lists:seq(0, ?MAP_SIZE - 1)),
    io:format(File, "    ].~n~n", []),
    
    % Write helper functions
    io:format(File, "get_cell_at(X, Y) when X >= 0, X < 16, Y >= 0, Y < 16 ->~n", []),
    io:format(File, "    Map = get_map(),~n", []),
    io:format(File, "    Row = lists:nth(X + 1, Map),~n", []),
    io:format(File, "    lists:nth(Y + 1, Row).~n~n", []),
    
    io:format(File, "get_tile_at(X, Y) ->~n", []),
    io:format(File, "    {TileType, _, _, _} = get_cell_at(X, Y),~n", []),
    io:format(File, "    TileType.~n~n", []),
    
    io:format(File, "get_powerup_at(X, Y) ->~n", []),
    io:format(File, "    {_, PowerupType, _, _} = get_cell_at(X, Y),~n", []),
    io:format(File, "    PowerupType.~n~n", []),
    
    io:format(File, "get_bomb_at(X, Y) ->~n", []),
    io:format(File, "    {_, _, BombType, _} = get_cell_at(X, Y),~n", []),
    io:format(File, "    BombType.~n~n", []),
    
    io:format(File, "get_player_at(X, Y) ->~n", []),
    io:format(File, "    {_, _, _, PlayerID} = get_cell_at(X, Y),~n", []),
    io:format(File, "    PlayerID.~n~n", []),
    
    io:format(File, "get_player_starts() ->~n", []),
    io:format(File, "    [{player_1, 1, 1}, {player_2, 1, 14}, {player_3, 14, 1}, {player_4, 14, 14}].~n", []),
    
    file:close(File),
    io:format("📄 Unified grid map exported to ~s~n", [Filename]).

%% Helper function to format cells for export
format_cell_for_export({TileType, PowerupType, BombType, PlayerID}) ->
    io_lib:format("{~w, ~w, ~w, ~w}", [TileType, PowerupType, BombType, PlayerID]).

%% ===================================================================
%% Test Functions
%% ===================================================================

test_generation() ->
    io:format("🧪 Testing unified grid map generation...~n"),
    
    % Generate map with full state
    MapState = generate_map_with_powerups(),
    Grid = MapState#map_state.grid,
    
    % Test basic properties
    BorderCell = get_cell_at(Grid, 0, 0),
    CenterCell = get_cell_at(Grid, 8, 8),
    PlayerCell = get_cell_at(Grid, 1, 1),
    
    io:format("Border cell: ~w~n", [BorderCell]),
    io:format("Player start: ~w~n", [PlayerCell]),
    io:format("Center area: ~w~n", [CenterCell]),
    
    % Test component extraction
    io:format("Border tile type: ~w~n", [get_tile_type(BorderCell)]),
    io:format("Player at start: ~w~n", [get_player_id(PlayerCell)]),
    
    % Visualize
    visualize_map(Grid),

    % Send to Python port for graphical interface
    io:format("Starting Python port for visualization...~n"),   % for debugging
    start_grid_port(),
    send_grid(Grid),
    
    % Export for testing
    %export_map(Grid, "test_unified_map.erl"),
    
    io:format("✅ Unified grid test complete!~n"),
    Grid.