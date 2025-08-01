
-behaviour(gen_server).

%% API
-export([start_link/1, get_current_map/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("mnesia_records.hrl").

-define(MAP_SIZE, 16).

-record(state, {
    gn_graphics_servers = [],     % List of {Node, Pid} for GN graphics servers
    python_port,                  % Port to Python visualizer
    current_map_state,            % Current unified map state
    gn_nodes,                     % List of GN nodes
    subscribed_tables = [],       % List of tables subscribed to
    update_counter = 0,           % Counter for updates (debugging)
    movement_states = #{},        % Track active player movements
    bomb_movements = #{}          % Track active bomb movements
}).

%%%===================================================================
%%% API
%%%===================================================================

%% Starts the central graphics server
-spec start_link(list()) -> {ok, pid()} | ignore | {error, term()}.
start_link(GNNodes) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [GNNodes], []).

%% Get current map state
-spec get_current_map() -> term().
get_current_map() ->
    gen_server:call(?MODULE, get_current_map).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% Initialize the graphics server
init([GNNodes]) ->
    io:format("🎨 CN Graphics Server starting...~n"),
   
    % Create initial state
    State = #state{gn_nodes = GNNodes},
   
    % Set up mnesia subscriptions (not sure if needs delay)
    erlang:send(self(), setup_subscriptions),
   
    % Spawn GN graphics servers (not sure if needs delay)
    erlang:send_after(30, self(), spawn_gn_servers),
   
    % Create Python port
    erlang:send(self(), create_python_port),
   
    % Start periodic updates (not sure if needs delay)
    erlang:send_after(25, self(), periodic_update),
   
    io:format("✅ CN Graphics Server initialized~n"),
    {ok, State}.

%% Handle synchronous calls
handle_call(get_current_map, _From, State) ->
    {reply, State#state.current_map_state, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% Handle asynchronous casts
handle_cast(force_update, State) ->
    io:format("🔄 Updating map state...~n"),
    NewMapState = create_current_map_state(),
    UpdatedState = State#state{current_map_state = NewMapState},
    send_map_to_all_targets(UpdatedState),
    {noreply, UpdatedState};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Handle messages
handle_info(setup_subscriptions, State) ->
    io:format("📡 Setting up mnesia subscriptions...~n"),
    Tables = get_all_tables(),
    SubscribedTables = setup_mnesia_subscriptions(Tables),
    io:format("✅ Subscribed to tables: ~p~n", [SubscribedTables]),
    {noreply, State#state{subscribed_tables = SubscribedTables}};

%% Not sure if its right...
handle_info(spawn_gn_servers, State) ->
    io:format("🚀 Spawning GN graphics servers...~n"),
    GNServers = spawn_all_gn_graphics_servers(State#state.gn_nodes),
    io:format("✅ Spawned GN graphics servers: ~p~n", [length(GNServers)]),
    {noreply, State#state{gn_graphics_servers = GNServers}};

handle_info(create_python_port, State) ->
    io:format("🐍 Creating Python port...~n"),
    Port = create_python_visualizer_port(),
    % Create initial map state
    InitialMapState = create_current_map_state(),
    UpdatedState = State#state{
        python_port = Port,
        current_map_state = InitialMapState
    },
    % Send initial state
    send_map_to_all_targets(UpdatedState),
    io:format("✅ Python port created and initial map sent~n"),
    {noreply, UpdatedState};

handle_info(periodic_update, State) ->
    % Periodic update every 25 milliseconds
    NewMapState = create_current_map_state(),
    UpdatedState = State#state{
        current_map_state = NewMapState,
        update_counter = State#state.update_counter + 1
    },
   
    % Only send if something changed or every 10th update
    ShouldSend = (NewMapState =/= State#state.current_map_state) orelse
                 (State#state.update_counter rem 10 == 0),
   
    if ShouldSend ->
        send_map_to_all_targets(UpdatedState),
        io:format("🔄 Periodic update #~w sent~n", [UpdatedState#state.update_counter]);
    true -> ok
    end,
   
    % Schedule next update
    erlang:send_after(25, self(), periodic_update),
    {noreply, UpdatedState};

% Handle mnesia table events
handle_info({mnesia_table_event, {write, Table, Record, ActivityId}}, State) ->
    case Record of
        #mnesia_players{} ->
            % Check if this is a movement-related update
            case detect_player_movement_change(Record, State#state.current_map_state) of
                {movement_started, PlayerData} ->
                    send_movement_confirmation_to_python(State, player, PlayerData),
                    handle_mnesia_update(State);
                no_movement_change ->
                    io:format("📝 Mnesia write: ~w on table ~w~n", [element(2, Record), Table]),
                    handle_mnesia_update(State)
            end;
        #mnesia_bombs{} ->
            % Check if this is a bomb movement-related update
            case detect_bomb_movement_change(Record, State#state.current_map_state) of
                {movement_started, BombData} ->
                    send_movement_confirmation_to_python(State, bomb, BombData),
                    handle_mnesia_update(State);
                no_movement_change ->
                    io:format("📝 Mnesia write: ~w on table ~w~n", [element(2, Record), Table]),
                    handle_mnesia_update(State)
            end;
        _ ->
            io:format("📝 Mnesia write: ~w on table ~w~n", [element(2, Record), Table]),
            handle_mnesia_update(State)
    end;

handle_info({mnesia_table_event, {delete, Table, Key, ActivityId}}, State) ->
    io:format("🗑️ Mnesia delete: ~w from table ~w~n", [Key, Table]),
    handle_mnesia_update(State);

handle_info({mnesia_table_event, _Event}, State) ->
    % Other mnesia events
    handle_mnesia_update(State);

% Handle Python port messages
handle_info({Port, {data, Data}}, State) when Port == State#state.python_port ->
    io:format("🐍 Message from Python: ~p~n", [Data]),
    {noreply, State};

handle_info({Port, closed}, State) when Port == State#state.python_port ->
    io:format("⚠️ Python port closed, attempting to restart...~n"),
    NewPort = create_python_visualizer_port(),
    {noreply, State#state{python_port = NewPort}};

handle_info(Info, State) ->
    io:format("ℹ️ Unexpected message: ~p~n", [Info]),
    {noreply, State}.

%% Cleanup on termination
terminate(Reason, State) ->
    io:format("🛑 CN Graphics Server terminating: ~p~n", [Reason]),
   
    % Close Python port
    if State#state.python_port =/= undefined ->
        port_close(State#state.python_port);
    true -> ok
    end,
   
    % Terminate GN graphics servers
    lists:foreach(fun({_Node, Pid}) ->
        if is_pid(Pid) andalso is_process_alive(Pid) ->
            exit(Pid, shutdown);
        true -> ok
        end
    end, State#state.gn_graphics_servers),
   
    ok.

%% Handle code changes
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Detect player movement changes
detect_player_movement_change(NewRecord, CurrentMapState) ->
    #mnesia_players{
        player_number = PlayerNum,
        position = [X, Y],
        direction = Direction,
        movement = Movement,
        speed = Speed
    } = NewRecord,
   
    % Check if movement field changed from false to {true, TimerRef}
    case Movement of
        {true, _TimerRef} when Direction =/= none ->
            % Movement just started - calculate destination
            Destination = calculate_destination([X, Y], Direction),
            PlayerData = #{
                player_id => PlayerNum,
                from_pos => [X, Y],
                to_pos => Destination,
                direction => Direction,
                speed => Speed,
                movement_confirmed => true
            },
            {movement_started, PlayerData};
        _ ->
            no_movement_change
    end.

%% Detect bomb movement changes
detect_bomb_movement_change(NewRecord, CurrentMapState) ->
    #mnesia_bombs{
        position = [X, Y],
        movement = Movement,
        direction = Direction,
        type = Type,
        owner = Owner,
        radius = Radius
    } = NewRecord,
   
    % Check if movement field changed from false to {true, TimerRef}
    case Movement of
        {true, _TimerRef} when Direction =/= none ->
            % Bomb movement just started (kicked!)
            Destination = calculate_destination([X, Y], Direction),
            BombData = #{
                bomb_id => [X, Y], % Use position as ID for bombs
                from_pos => [X, Y],
                to_pos => Destination,
                direction => Direction,
                type => Type,
                owner => Owner,
                radius => Radius,
                movement_confirmed => true
            },
            {movement_started, BombData};
        _ ->
            no_movement_change
    end.

%% Calculate destination position
calculate_destination([X, Y], Direction) ->
    case Direction of
        up -> [X, Y-1];
        down -> [X, Y+1];
        left -> [X-1, Y];
        right -> [X+1, Y]
    end.

%% Send movement confirmation to Python
send_movement_confirmation_to_python(State, EntityType, EntityData) ->
    if State#state.python_port =/= undefined ->
        try
            ConfirmationMsg = [movement_confirmation, #{
                entity_type => EntityType,
                entity_data => EntityData
            }],
            MsgBinary = term_to_binary(ConfirmationMsg),
            port_command(State#state.python_port, MsgBinary),
            case EntityType of
                player ->
                    io:format("🏃 Player movement confirmation sent for player ~w~n",
                             [maps:get(player_id, EntityData)]);
                bomb ->
                    io:format("💣 Bomb movement confirmation sent for bomb at ~w~n",
                             [maps:get(from_pos, EntityData)])
            end
        catch
            _:Error ->
                io:format("❌ Error sending movement confirmation: ~p~n", [Error])
        end;
    true ->
        ok
    end.

%% Handle mnesia updates by recreating map state
handle_mnesia_update(State) ->
    NewMapState = create_current_map_state(),
    UpdatedState = State#state{current_map_state = NewMapState},
    send_map_to_all_targets(UpdatedState),
    {noreply, UpdatedState}.

%% Get all table names that we need to monitor
get_all_tables() ->
    [gn1_tiles, gn2_tiles, gn3_tiles, gn4_tiles,
     gn1_bombs, gn2_bombs, gn3_bombs, gn4_bombs,
     gn1_powerups, gn2_powerups, gn3_powerups, gn4_powerups,
     gn1_players, gn2_players, gn3_players, gn4_players].

%% Set up mnesia subscriptions for all relevant tables
setup_mnesia_subscriptions(Tables) ->
    lists:foldl(fun(Table, Acc) ->
        case mnesia:subscribe({table, Table, simple}) of
            {ok, _} ->
                io:format("✅ Subscribed to table: ~w~n", [Table]),
                [Table | Acc];
            {error, Reason} ->
                io:format("❌ Failed to subscribe to ~w: ~p~n", [Table, Reason]),
                Acc
        end
    end, [], Tables).

%% Spawn graphics servers on all GN nodes
spawn_all_gn_graphics_servers(GNNodes) ->
    lists:foldl(fun(Node, Acc) ->
        case spawn_gn_graphics_server(Node) of
            {ok, Pid} -> [{Node, Pid} | Acc];
            {error, Reason} ->
                io:format("❌ Failed to spawn GN server on ~w: ~p~n", [Node, Reason]),
                Acc
        end
    end, [], GNNodes).

%% Spawn a single GN graphics server on the specified node
spawn_gn_graphics_server(Node) ->
    try
        % Spawn the GN graphics server on the remote node
        Pid = rpc:call(Node, gn_graphics_server, start_link, [node()]),
        case Pid of
            {ok, ActualPid} ->
                link(ActualPid),
                io:format("✅ GN graphics server started on ~w: ~p~n", [Node, ActualPid]),
                {ok, ActualPid};
            Error ->
                io:format("❌ Failed to start GN server on ~w: ~p~n", [Node, Error]),
                {error, Error}
        end
    catch
        _Error ->
            io:format("❌ Exception spawning GN server on ~w: ~p~n", [Node, Error]),
            {error, Error}
    end.

%% Create Python visualizer port
create_python_visualizer_port() ->
    try
        Port = open_port({spawn, "python3 map_live_port.py"},
                        [binary, exit_status, {packet, 4}]),
        io:format("✅ Python port created: ~p~n", [Port]),
        Port
    catch
        _:Error ->
            io:format("❌ Failed to create Python port: ~p~n", [Error]),
            undefined
    end.

%% Create current unified map state from all mnesia tables
create_current_map_state() ->
    try
        % Initialize empty map
        EmptyMap = create_empty_map(),
       
        % Add tiles from all GN tables
        MapWithTiles = add_tiles_to_map(EmptyMap),
       
        % Add powerups
        MapWithPowerups = add_powerups_to_map(MapWithTiles),
       
        % Add bombs
        MapWithBombs = add_bombs_to_map(MapWithPowerups),
       
        % Add players
        FinalMap = add_players_to_map(MapWithBombs),
       
        FinalMap
    catch
        _:Error ->
            io:format("❌ Error creating map state: ~p~n", [Error])
    end.

%% Create empty 16x16 map with free tiles
create_empty_map() ->
    [[{free, none, none, none, none, none} || _ <- lists:seq(1, ?MAP_SIZE)]
     || _ <- lists:seq(1, ?MAP_SIZE)].

%% Add tiles from all GN tables to the map
add_tiles_to_map(Map) ->
    TileTables = [gn1_tiles, gn2_tiles, gn3_tiles, gn4_tiles],
    lists:foldl(fun(Table, AccMap) ->
        add_tiles_from_table(Table, AccMap)
    end, Map, TileTables).

%% Add tiles from a specific table
add_tiles_from_table(Table, Map) ->
    Fun = fun() ->
        mnesia:select(Table, [{#mnesia_tiles{_ = '_'}, [], ['$_']}])
    end,
   
    case mnesia:activity(transaction, Fun) of
        TileRecords when is_list(TileRecords) ->
            lists:foldl(fun(TileRecord, AccMap) ->
                update_map_with_tile(AccMap, TileRecord)
            end, Map, TileRecords);
        Error ->
            io:format("❌ Error reading table ~w: ~p~n", [Table, Error]),
            Map
    end.

%% Update map with tile information
update_map_with_tile(Map, TileRecord) ->
    #mnesia_tiles{position = [X, Y], type = Type, contains = Contains} = TileRecord,
   
    if X >= 0, X < ?MAP_SIZE, Y >= 0, Y < ?MAP_SIZE ->
        Row = lists:nth(X + 1, Map),
        OldCell = lists:nth(Y + 1, Row),
        NewCell = update_cell_tile(OldCell, Type, Contains),
        NewRow = replace_list_element(Row, Y + 1, NewCell),
        replace_list_element(Map, X + 1, NewRow);
    true ->
        io:format("⚠️ Invalid tile position: ~w, ~w~n", [X, Y]),
        Map
    end.

%% Add powerups from all tables
add_powerups_to_map(Map) ->
    PowerupTables = [gn1_powerups, gn2_powerups, gn3_powerups, gn4_powerups],
    lists:foldl(fun(Table, AccMap) ->
        add_powerups_from_table(Table, AccMap)
    end, Map, PowerupTables).

%% Add powerups from a specific table
add_powerups_from_table(Table, Map) ->
    Fun = fun() ->
        mnesia:select(Table, [{#mnesia_powerups{_ = '_'}, [], ['$_']}])
    end,
   
    case mnesia:activity(transaction, Fun) of
        PowerupRecords when is_list(PowerupRecords) ->
            lists:foldl(fun(PowerupRecord, AccMap) ->
                update_map_with_powerup(AccMap, PowerupRecord)
            end, Map, PowerupRecords);
        Error ->
            io:format("❌ Error reading powerup table ~w: ~p~n", [Table, Error]),
            Map
    end.

%% Update map with powerup information
update_map_with_powerup(Map, PowerupRecord) ->
    #mnesia_powerups{position = [X, Y], type = Type} = PowerupRecord,
   
    if X >= 0, X < ?MAP_SIZE, Y >= 0, Y < ?MAP_SIZE ->
        Row = lists:nth(X + 1, Map),
        OldCell = lists:nth(Y + 1, Row),
        NewCell = update_cell_powerup(OldCell, Type),
        NewRow = replace_list_element(Row, Y + 1, NewCell),
        replace_list_element(Map, X + 1, NewRow);
    true ->
        io:format("⚠️ Invalid powerup position: ~w, ~w~n", [X, Y]),
        Map
    end.

%% Add bombs from all tables
add_bombs_to_map(Map) ->
    BombTables = [gn1_bombs, gn2_bombs, gn3_bombs, gn4_bombs],
    lists:foldl(fun(Table, AccMap) ->
        add_bombs_from_table(Table, AccMap)
    end, Map, BombTables).

%% Add bombs from a specific table, include speed info
add_bombs_from_table(Table, Map) ->
    Fun = fun() ->
        mnesia:select(Table, [{#mnesia_bombs{_ = '_'}, [], ['$_']}])
    end,
   
    case mnesia:activity(transaction, Fun) of
        BombRecords when is_list(BombRecords) ->
            lists:foldl(fun(BombRecord, AccMap) ->
                update_map_with_bomb(AccMap, BombRecord)
            end, Map, BombRecords);
        Error ->
            io:format("❌ Error reading bomb table ~w: ~p~n", [Table, Error]),
            Map
    end.

%% Update map with bomb information
update_map_with_bomb(Map, BombRecord) ->
    #mnesia_bombs{position = [X, Y], type = Type, ignited = Ignited,
                  status = Status, radius = Radius, owner = Owner} = BombRecord,
   
    if X >= 0, X < ?MAP_SIZE, Y >= 0, Y < ?MAP_SIZE ->
        Row = lists:nth(X + 1, Map),
        OldCell = lists:nth(Y + 1, Row),
        BombInfo = {Type, Ignited, Status, Radius, Owner},
        NewCell = update_cell_bomb(OldCell, BombInfo),
        NewRow = replace_list_element(Row, Y + 1, NewCell),
        replace_list_element(Map, X + 1, NewRow);
    true ->
        io:format("⚠️ Invalid bomb position: ~w, ~w~n", [X, Y]),
        Map
    end.

%% Add players from all tables, include speed
add_players_to_map(Map) ->
    PlayerTables = [gn1_players, gn2_players, gn3_players, gn4_players],
    lists:foldl(fun(Table, AccMap) ->
        add_players_from_table(Table, AccMap)
    end, Map, PlayerTables).

%% Add players from a specific table
add_players_from_table(Table, Map) ->
    Fun = fun() ->
        mnesia:select(Table, [{#mnesia_players{_ = '_'}, [], ['$_']}])
    end,
   
    case mnesia:activity(transaction, Fun) of
        PlayerRecords when is_list(PlayerRecords) ->
            lists:foldl(fun(PlayerRecord, AccMap) ->
                update_map_with_player(AccMap, PlayerRecord)
            end, Map, PlayerRecords);
        Error ->
            io:format("❌ Error reading player table ~w: ~p~n", [Table, Error]),
            Map
    end.

%% Update map with player information, with speed
update_map_with_player(Map, PlayerRecord) ->
    #mnesia_players{position = [X, Y], player_number = PlayerID,
                    life = Life, speed = Speed} = PlayerRecord,
   
    if X >= 0, X < ?MAP_SIZE, Y >= 0, Y < ?MAP_SIZE ->
        Row = lists:nth(X + 1, Map),
        OldCell = lists:nth(Y + 1, Row),
        PlayerInfo = {PlayerID, Life, Speed},
        NewCell = update_cell_player(OldCell, PlayerInfo),
        NewRow = replace_list_element(Row, Y + 1, NewCell),
        replace_list_element(Map, X + 1, NewRow);
    true ->
        io:format("⚠️ Invalid player position: ~w, ~w~n", [X, Y]),
        Map
    end.

%% Update cell with tile information
update_cell_tile({_, Powerup, Bomb, Player, Explosion, Special}, TileType, Contains) ->
    % If tile contains a powerup, update powerup field
    ActualPowerup = if Contains =/= none -> Contains; true -> Powerup end,
    {TileType, ActualPowerup, Bomb, Player, Explosion, Special}.

%% Update cell with powerup information
update_cell_powerup({Tile, _, Bomb, Player, Explosion, Special}, PowerupType) ->
    {Tile, PowerupType, Bomb, Player, Explosion, Special}.

%% Update cell with bomb information
update_cell_bomb({Tile, Powerup, _, Player, Explosion, Special}, BombInfo) ->
    {Tile, Powerup, BombInfo, Player, Explosion, Special}.

%% Update cell with player information
update_cell_player({Tile, Powerup, Bomb, _, Explosion, Special}, PlayerInfo) ->
    {Tile, Powerup, Bomb, PlayerInfo, Explosion, Special}.

%% Replace element in list at specific position (1-indexed)
replace_list_element(List, Pos, NewElement) ->
    {Before, [_|After]} = lists:split(Pos - 1, List),
    Before ++ [NewElement] ++ After.

%% Send map to all targets (Python and GN servers)
send_map_to_all_targets(State) ->
    % Send to Python visualizer
    send_map_to_python(State),
   
    % Send to GN graphics servers
    send_map_to_gn_servers(State).

%% Send map to Python visualizer
send_map_to_python(State) ->
    if State#state.python_port =/= undefined andalso
       State#state.current_map_state =/= undefined ->
        try
            % Convert map to binary Erlang term
            MapBinary = term_to_binary(State#state.current_map_state),
            port_command(State#state.python_port, MapBinary),
            io:format("🐍 Map sent to Python visualizer~n")
        catch
            _:Error ->
                io:format("❌ Error sending to Python: ~p~n", [Error])
        end;
    true ->
        io:format("⚠️ Python port or map state not ready~n")
    end.

%% Send map to all GN graphics servers
send_map_to_gn_servers(State) ->
    lists:foreach(fun({Node, Pid}) ->
        if is_pid(Pid) andalso is_process_alive(Pid) ->
            try
                gen_server:cast(Pid, {map_update, State#state.current_map_state}),
                io:format("📡 Map sent to GN server on ~w~n", [Node])
            catch
                _:Error ->
                    io:format("❌ Error sending to GN server on ~w: ~p~n", [Node, Error])
            end;
        true ->
            io:format("⚠️ GN server on ~w not alive~n", [Node])
        end
    end, State#state.gn_graphics_servers).