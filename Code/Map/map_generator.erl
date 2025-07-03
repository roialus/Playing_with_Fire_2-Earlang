
-module(map_generator).

-export([generate_map/0, generate_map/1, visualize_map/1, 
         get_tile_at/3, export_map/2]).

%% Tile type definitions
-define(FREE, 0).         % Free spot (no tile) - walkable
-define(BREAKABLE, 1).    % Breakable tile - destructible in 1 explosion
-define(UNBREAKABLE, 2).  % Unbreakable wall - permanent obstacle
-define(STRONG, 3).       % Strong tile - needs 2 explosions to break
-define(PLAYER_START, 4). % Player starting position

-define(MAP_SIZE, 16).

-record(map_state, {
    size = ?MAP_SIZE,
    grid,
    corners = [{1, 1}, {1, 14}, {14, 1}, {14, 14}],
    steiner_paths = []
}).

%% ===================================================================
%% Public API - Simple Map Generation
%% ===================================================================

generate_map() ->
    generate_map(#{}).

generate_map(Options) ->
    % Default options for map tiles
    DefaultOptions = #{
        steiner_breakable_chance => 0.3,
        breakable_prob => 0.3,
        strong_prob => 0.15,
        unbreakable_prob => 0.25,
        num_random_terminals => 2
    },
    Opts = maps:merge(DefaultOptions, Options),
    
    % For debugging
    io:format("🗺️  Generating ~wx~w Erlang Map...~n", [?MAP_SIZE, ?MAP_SIZE]),
    
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
    FinalMap = mark_player_starts(MapState3),
    
    % For debugging
    io:format("✅ Map generation complete!~n"),
    FinalMap#map_state.grid.

%% ===================================================================
%% Map Initialization
%% ===================================================================

initialize_map() ->
    % Create 16x16 grid filled with FREE spots
    Grid = array:new(?MAP_SIZE, {default, ?FREE}),
    EmptyGrid = array:map(fun(_, _) -> 
        array:new(?MAP_SIZE, {default, ?FREE}) 
    end, Grid),
    
    % Set borders to UNBREAKABLE
    BorderGrid = set_borders(EmptyGrid),
    
    % Clear player areas
    ClearGrid = clear_player_areas(BorderGrid),
    
    #map_state{grid = ClearGrid}.

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
    X >= 1 andalso X < ?MAP_SIZE - 1 andalso Y >= 1 andalso Y < ?MAP_SIZE - 1.  % Ensure within bounds

%% ===================================================================
%% Graph Creation for Steiner Tree
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
    
    #{nodes => Nodes, edges => Edges}.  % Create graph structure

%% ===================================================================
%% Terminal Selection
%% ===================================================================

select_terminals(NumRandom) ->
    Corners = [{1, 1}, {1, 14}, {14, 1}, {14, 14}],
    
    % Add random terminals
    RandomTerminals = select_random_terminals(NumRandom, Corners, []),
    
    Corners ++ RandomTerminals. % Combine corners with random terminals

select_random_terminals(0, _Existing, Acc) -> Acc;  % Base case: no more terminals to select
select_random_terminals(N, Existing, Acc) ->
    X = rand:uniform(?MAP_SIZE - 6) + 2, % Range 3 to MAP_SIZE-4
    Y = rand:uniform(?MAP_SIZE - 6) + 2,    
    
    case is_valid_terminal({X, Y}, Existing ++ Acc) of
        true -> select_random_terminals(N - 1, Existing, [{X, Y} | Acc]);   % Valid terminal found
        false -> select_random_terminals(N, Existing, Acc) % Try again
    end.

is_valid_terminal({X, Y}, ExistingTerminals) ->
    % Check minimum distance of 3 from existing terminals
    lists:all(fun({TX, TY}) ->
        abs(X - TX) >= 3 orelse abs(Y - TY) >= 3
    end, ExistingTerminals).

%% ===================================================================
%% Simplified Steiner Tree Generation
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
     T1 <- Terminals, T2 <- Terminals, T1 < T2].    % Calculate pairwise distances

manhattan_distance({X1, Y1}, {X2, Y2}) ->
    abs(X1 - X2) + abs(Y1 - Y2).    % Calculate Manhattan distance

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
    Root1 = find_root(T1, UF),  % Find root of T1
    Root2 = find_root(T2, UF),  % Find root of T2
    
    case Root1 =:= Root2 of
        true -> 
            % Creates cycle, skip this edge
            select_mst_edges(Rest, UF, Acc);
        false ->
            % Add edge and union the sets
            NewUF = maps:put(Root1, Root2, UF),
            select_mst_edges(Rest, NewUF, [{T1, T2} | Acc])
    end.

find_root(Node, UF) ->
    case maps:get(Node, UF) of
        Node -> Node;   % Node is its own root
        Parent -> find_root(Parent, UF) % Path compression
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
    
    lists:usort(XPath ++ YPath).    % Combine paths

extract_steiner_paths(SteinerEdges) ->
    lists:usort(lists:flatten(SteinerEdges)).   % Extract unique positions from edges

%% ===================================================================
%% Apply Steiner Tree to Map
%% ===================================================================

apply_steiner_tree(MapState, SteinerPaths, BreakableChance) ->
    Grid = MapState#map_state.grid, % Initialize grid
    
    NewGrid = lists:foldl(fun({X, Y}, AccGrid) ->
        case is_valid_position(X, Y) andalso not is_in_player_area(X, Y) of
            true ->
                % Steiner paths can be FREE or BREAKABLE
                TileType = case rand:uniform() =< BreakableChance of
                    true -> ?BREAKABLE;
                    false -> ?FREE
                end,
                Row = array:get(X, AccGrid),   
                NewRow = array:set(Y, TileType, Row),
                array:set(X, NewRow, AccGrid);  % Update grid
            false ->
                AccGrid % Invalid position, keep original grid
        end
    end, Grid, SteinerPaths),
    
    MapState#map_state{grid = NewGrid, steiner_paths = SteinerPaths}.   % Update map state

is_in_player_area(X, Y) ->
    Corners = [{1, 1}, {1, 14}, {14, 1}, {14, 14}],
    lists:any(fun({CX, CY}) ->
        abs(X - CX) =< 1 andalso abs(Y - CY) =< 1
    end, Corners).  % Check if in player start area

%% ===================================================================
%% Fill Remaining Areas
%% ===================================================================

fill_remaining_areas(MapState, Options) ->
    Grid = MapState#map_state.grid,
    SteinerPaths = MapState#map_state.steiner_paths,    
    
    BreakableProb = maps:get(breakable_prob, Options),
    StrongProb = maps:get(strong_prob, Options),
    UnbreakableProb = maps:get(unbreakable_prob, Options),
    
    % Find all non-Steiner, non-player area positions
    AvailablePositions = get_available_positions(SteinerPaths),
    
    % Fill with random tile types
    NewGrid = lists:foldl(fun({X, Y}, AccGrid) ->
        TileType = select_random_tile_type(BreakableProb, StrongProb, UnbreakableProb),
        Row = array:get(X, AccGrid),
        NewRow = array:set(Y, TileType, Row),
        array:set(X, NewRow, AccGrid)
    end, Grid, AvailablePositions),
    
    MapState#map_state{grid = NewGrid}.

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
        Rand =< BreakableProb -> ?BREAKABLE;
        Rand =< BreakableProb + StrongProb -> ?STRONG;
        Rand =< BreakableProb + StrongProb + UnbreakableProb -> ?UNBREAKABLE;
        true -> ?FREE
    end.

%% ===================================================================
%% Mark Player Starts
%% ===================================================================

mark_player_starts(MapState) ->
    Grid = MapState#map_state.grid,
    Corners = [{1, 1}, {1, 14}, {14, 1}, {14, 14}],
    
    NewGrid = lists:foldl(fun({X, Y}, AccGrid) ->
        Row = array:get(X, AccGrid),
        NewRow = array:set(Y, ?PLAYER_START, Row),
        array:set(X, NewRow, AccGrid)
    end, Grid, Corners),
    
    MapState#map_state{grid = NewGrid}.

%% ===================================================================
%% Utility Functions
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

%% Visualize the map in console for debugging
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
%% Export Map to Erlang Module
%% ===================================================================
export_map(Grid, Filename) ->
    {ok, File} = file:open(Filename, [write]),
    
    % Write header
    io:format(File, "%% Generated 16x16 Bomberman Map~n", []),
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

%% ===================================================================
%% Usage Examples
%% ===================================================================

%% Simple usage:
%% 1> Grid = map_generator:generate_map().
%% 2> map_generator:visualize_map(Grid).
%% 3> map_generator:export_map(Grid, "my_map.erl").

%% With custom options:
%% 1> Options = #{steiner_breakable_chance => 0.5, breakable_prob => 0.4}.
%% 2> Grid = map_generator:generate_map(Options).
%% 3> TileType = map_generator:get_tile_at(Grid, 5, 5).

%% Test map generation:
test_generation() ->
    io:format("🧪 Testing map generation...~n"),
    
    % Generate map
    Grid = generate_map(),
    
    % Test basic properties
    BorderTile = get_tile_at(Grid, 0, 0),
    CenterTile = get_tile_at(Grid, 8, 8),
    PlayerTile = get_tile_at(Grid, 1, 1),
    
    io:format("Border tile: ~w (should be ~w)~n", [BorderTile, ?UNBREAKABLE]),
    io:format("Player start: ~w (should be ~w)~n", [PlayerTile, ?PLAYER_START]),
    io:format("Center area: ~w~n", [CenterTile]),
    
    % Visualize
    visualize_map(Grid),
    
    % Export for testing
    export_map(Grid, "test_map.erl"),
    
    io:format("✅ Test complete!~n"),
    Grid.
