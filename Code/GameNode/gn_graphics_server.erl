-module(gn_graphics_server).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("mnesia_records.hrl").

-record(state, {
    cn_node,                    % Central node
    python_port,                % Port to local Python visualizer
    update_counter = 0,         % Update counter
    current_map_state,          % Current map state for comparison
    subscribed_tables = [],     % List of subscribed mnesia tables
    movement_states = #{},      % Track active player movements
    bomb_movements = #{}        % Track active bomb movements
}).

%%%===================================================================
%%% API
%%%===================================================================

%% Starts the GN graphics server
-spec start_link(node()) -> {ok, pid()} | ignore | {error, term()}.
start_link(CNNode) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [CNNode], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% Initialize the GN graphics server
init([CNNode]) ->
    io:format("🎮 Enhanced GN Graphics Server starting on ~w~n", [node()]),
    
    State = #state{cn_node = CNNode},
    
    % Set up mnesia subscriptions for local tables
    erlang:send(self(), setup_subscriptions),
    
    % Create Python port
    erlang:send_after(50, self(), create_python_port),
    
    % Start periodic updates
    erlang:send_after(75, self(), periodic_update),
    
    io:format("✅ Enhanced GN Graphics Server initialized~n"),
    {ok, State}.

%% Handle synchronous calls
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% Handle asynchronous casts
handle_cast({map_update, MapState}, State) ->
    % Received map update from CN graphics server - send to Python
    io:format("🗺️ GN received map update from CN, sending to Python~n"),
    send_map_to_python(State#state.python_port, MapState),
    
    NewState = State#state{
        current_map_state = MapState,
        update_counter = State#state.update_counter + 1
    },
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Handle messages
handle_info(setup_subscriptions, State) ->
    io:format("📡 Setting up local mnesia subscriptions...~n"),
    LocalTables = get_local_tables(),
    SubscribedTables = setup_mnesia_subscriptions(LocalTables),
    io:format("✅ Subscribed to local tables: ~p~n", [SubscribedTables]),
    {noreply, State#state{subscribed_tables = SubscribedTables}};

handle_info(create_python_port, State) ->
    % Create Python port for visualizer
    io:format("🐍 Creating enhanced Python visualizer port...~n"),
    Port = create_python_port(),
    
    % Create initial map state from local tables
    InitialMapState = create_local_map_state(),
    UpdatedState = State#state{
        python_port = Port,
        current_map_state = InitialMapState
    },
    
    % Send initial state
    send_map_to_python(Port, InitialMapState),
    io:format("✅ Enhanced Python port created and initial map sent~n"),
    {noreply, UpdatedState};

handle_info(periodic_update, State) ->
    % Periodic update every 25 milliseconds for local changes
    NewMapState = create_local_map_state(),
    UpdatedState = State#state{
        current_map_state = NewMapState,
        update_counter = State#state.update_counter + 1
    },
    
    % Only send if something changed locally or every 10th update
    ShouldSend = (NewMapState =/= State#state.current_map_state) orelse
                 (State#state.update_counter rem 10 == 0),
    
    if ShouldSend ->
        send_map_to_python(State#state.python_port, NewMapState),
        io:format("🔄 GN periodic update #~w sent~n", [UpdatedState#state.update_counter]);
    true -> ok
    end,
    
    % Schedule next update
    erlang:send_after(25, self(), periodic_update),
    {noreply, UpdatedState};

% Handle mnesia table events for movement detection
handle_info({mnesia_table_event, {write, Table, Record, ActivityId}}, State) ->
    case Record of
        #mnesia_players{} ->
            % Check if this is a movement-related update
            case detect_player_movement_change(Record, State#state.current_map_state) of
                {movement_started, PlayerData} ->
                    send_movement_confirmation_to_python(State, player, PlayerData),
                    handle_local_mnesia_update(State);
                no_movement_change ->
                    io:format("📝 Local player update: ~w on table ~w~n", [element(2, Record), Table]),
                    handle_local_mnesia_update(State)
            end;
        #mnesia_bombs{} ->
            % Check if this is a bomb movement-related update
            case detect_bomb_movement_change(Record, State#state.current_map_state) of
                {movement_started, BombData} ->
                    send_movement_confirmation_to_python(State, bomb, BombData),
                    handle_local_mnesia_update(State);
                no_movement_change ->
                    io:format("📝 Local bomb update: ~w on table ~w~n", [element(2, Record), Table]),
                    handle_local_mnesia_update(State)
            end;
        _ ->
            io:format("📝 Local mnesia write: ~w on table ~w~n", [element(2, Record), Table]),
            handle_local_mnesia_update(State)
    end;

handle_info({mnesia_table_event, {delete, Table, Key, ActivityId}}, State) ->
    io:format("🗑️ Local mnesia delete: ~w from table ~w~n", [Key, Table]),
    handle_local_mnesia_update(State);

handle_info({mnesia_table_event, _Event}, State) ->
    % Other mnesia events
    handle_local_mnesia_update(State);

% Handle Python port messages
handle_info({Port, {data, Data}}, State) when Port == State#state.python_port ->
    io:format("🐍 Message from Python: ~p~n", [Data]),
    {noreply, State};

handle_info({Port, closed}, State) when Port == State#state.python_port ->
    io:format("⚠️ Python port closed, restarting...~n"),
    NewPort = create_python_port(),
    {noreply, State#state{python_port = NewPort}};

handle_info(_Info, State) ->
    {noreply, State}.

%% Cleanup on termination
terminate(_Reason, State) ->
    io:format("🛑 Enhanced GN Graphics Server terminating~n"),
    if State#state.python_port =/= undefined ->
        port_close(State#state.python_port);
    true -> ok
    end,
    ok.

%% Handle code changes
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Movement Detection Functions (Similar to CN)
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
                    io:format("🏃 GN Player movement confirmation sent for player ~w~n",
                             [maps:get(player_id, EntityData)]);
                bomb ->
                    io:format("💣 GN Bomb movement confirmation sent for bomb at ~w~n",
                             [maps:get(from_pos, EntityData)])
            end
        catch
            _:Error ->
                io:format("❌ Error sending movement confirmation: ~p~n", [Error])
        end;
    true ->
        ok
    end.

%% Handle local mnesia updates
handle_local_mnesia_update(State) ->
    NewMapState = create_local_map_state(),
    UpdatedState = State#state{current_map_state = NewMapState},
    send_map_to_python(State#state.python_port, NewMapState),
    {noreply, UpdatedState}.

%%%===================================================================
%%% Local Map State Creation
%%%===================================================================

%% Get local table names for this GN node
get_local_tables() ->
    % Determine which GN node this is based on node name
    NodeName = atom_to_list(node()),
    if 
        string:str(NodeName, "gn1") > 0 ->
            [gn1_tiles, gn1_bombs, gn1_powerups, gn1_players];
        string:str(NodeName, "gn2") > 0 ->
            [gn2_tiles, gn2_bombs, gn2_powerups, gn2_players];
        string:str(NodeName, "gn3") > 0 ->
            [gn3_tiles, gn3_bombs, gn3_powerups, gn3_players];
        string:str(NodeName, "gn4") > 0 ->
            [gn4_tiles, gn4_bombs, gn4_powerups, gn4_players];
        true ->
            % Default fallback
            [gn1_tiles, gn1_bombs, gn1_powerups, gn1_players]
    end.

%% Set up mnesia subscriptions for local tables
setup_mnesia_subscriptions(Tables) ->
    lists:foldl(fun(Table, Acc) ->
        case mnesia:subscribe({table, Table, simple}) of
            {ok, _} ->
                io:format("✅ GN subscribed to table: ~w~n", [Table]),
                [Table | Acc];
            {error, Reason} ->
                io:format("❌ GN failed to subscribe to ~w: ~p~n", [Table, Reason]),
                Acc
        end
    end, [], Tables).

%% Create local map state from local mnesia tables
create_local_map_state() ->
    try
        % Get local tables
        LocalTables = get_local_tables(),
        
        % Create a partial map state with only local data
        EmptyMap = create_empty_map(),
        
        % Add local tiles, powerups, bombs, and players
        MapWithTiles = add_local_tiles_to_map(EmptyMap, LocalTables),
        MapWithPowerups = add_local_powerups_to_map(MapWithTiles, LocalTables),
        MapWithBombs = add_local_bombs_to_map(MapWithPowerups, LocalTables),
        FinalMap = add_local_players_to_map(MapWithBombs, LocalTables),
        
        FinalMap
    catch
        _:Error ->
            io:format("❌ Error creating local map state: ~p~n", [Error]),
            create_empty_map()
    end.

%% Create empty 16x16 map
create_empty_map() ->
    [[{free, none, none, none, none, none} || _ <- lists:seq(1, 16)]
     || _ <- lists:seq(1, 16)].

%% Add local tiles to map
add_local_tiles_to_map(Map, Tables) ->
    TileTable = lists:filter(fun(T) -> 
        string:str(atom_to_list(T), "_tiles") > 0 
    end, Tables),
    
    case TileTable of
        [Table] -> add_tiles_from_table(Table, Map);
        [] -> Map;
        _ -> Map
    end.

%% Add local powerups to map
add_local_powerups_to_map(Map, Tables) ->
    PowerupTable = lists:filter(fun(T) -> 
        string:str(atom_to_list(T), "_powerups") > 0 
    end, Tables),
    
    case PowerupTable of
        [Table] -> add_powerups_from_table(Table, Map);
        [] -> Map;
        _ -> Map
    end.

%% Add local bombs to map
add_local_bombs_to_map(Map, Tables) ->
    BombTable = lists:filter(fun(T) -> 
        string:str(atom_to_list(T), "_bombs") > 0 
    end, Tables),
    
    case BombTable of
        [Table] -> add_bombs_from_table(Table, Map);
        [] -> Map;
        _ -> Map
    end.

%% Add local players to map
add_local_players_to_map(Map, Tables) ->
    PlayerTable = lists:filter(fun(T) -> 
        string:str(atom_to_list(T), "_players") > 0 
    end, Tables),
    
    case PlayerTable of
        [Table] -> add_players_from_table(Table, Map);
        [] -> Map;
        _ -> Map
    end.

%% Add tiles from a specific table (reused from CN code)
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

%% Add powerups from a table
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

%% Add bombs from a table
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

%% Add players from a table
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

%% Map update helper functions (reused from CN code)
update_map_with_tile(Map, TileRecord) ->
    #mnesia_tiles{position = [X, Y], type = Type, contains = Contains} = TileRecord,
    
    if X >= 0, X < 16, Y >= 0, Y < 16 ->
        Row = lists:nth(X + 1, Map),
        OldCell = lists:nth(Y + 1, Row),
        NewCell = update_cell_tile(OldCell, Type, Contains),
        NewRow = replace_list_element(Row, Y + 1, NewCell),
        replace_list_element(Map, X + 1, NewRow);
    true ->
        io:format("⚠️ Invalid tile position: ~w, ~w~n", [X, Y]),
        Map
    end.

update_map_with_powerup(Map, PowerupRecord) ->
    #mnesia_powerups{position = [X, Y], type = Type} = PowerupRecord,
    
    if X >= 0, X < 16, Y >= 0, Y < 16 ->
        Row = lists:nth(X + 1, Map),
        OldCell = lists:nth(Y + 1, Row),
        NewCell = update_cell_powerup(OldCell, Type),
        NewRow = replace_list_element(Row, Y + 1, NewCell),
        replace_list_element(Map, X + 1, NewRow);
    true ->
        io:format("⚠️ Invalid powerup position: ~w, ~w~n", [X, Y]),
        Map
    end.

update_map_with_bomb(Map, BombRecord) ->
    #mnesia_bombs{position = [X, Y], type = Type, ignited = Ignited,
                  status = Status, radius = Radius, owner = Owner} = BombRecord,
    
    if X >= 0, X < 16, Y >= 0, Y < 16 ->
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

update_map_with_player(Map, PlayerRecord) ->
    #mnesia_players{position = [X, Y], player_number = PlayerID,
                    life = Life, speed = Speed} = PlayerRecord,
    
    if X >= 0, X < 16, Y >= 0, Y < 16 ->
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

%% Cell update helper functions
update_cell_tile({_, Powerup, Bomb, Player, Explosion, Special}, TileType, Contains) ->
    ActualPowerup = if Contains =/= none -> Contains; true -> Powerup end,
    {TileType, ActualPowerup, Bomb, Player, Explosion, Special}.

update_cell_powerup({Tile, _, Bomb, Player, Explosion, Special}, PowerupType) ->
    {Tile, PowerupType, Bomb, Player, Explosion, Special}.

update_cell_bomb({Tile, Powerup, _, Player, Explosion, Special}, BombInfo) ->
    {Tile, Powerup, BombInfo, Player, Explosion, Special}.

update_cell_player({Tile, Powerup, Bomb, _, Explosion, Special}, PlayerInfo) ->
    {Tile, Powerup, Bomb, PlayerInfo, Explosion, Special}.

%% Replace element in list at specific position (1-indexed)
replace_list_element(List, Pos, NewElement) ->
    {Before, [_|After]} = lists:split(Pos - 1, List),
    Before ++ [NewElement] ++ After.

%%%===================================================================
%%% Python Port Communication
%%%===================================================================

%% Create enhanced Python port for visualizer
create_python_port() ->
    try
        % Use the enhanced Python visualizer that handles movement confirmations
        Port = open_port({spawn, "python3 gn_map_live_enhanced.py"}, 
                        [binary, exit_status, {packet, 4}]),
        io:format("✅ Enhanced Python visualizer port created~n"),
        Port
    catch
        _:Error ->
            io:format("❌ Failed to create Python port: ~p~n", [Error]),
            undefined
    end.

%% Send map data to Python visualizer
send_map_to_python(undefined, _MapState) ->
    io:format("⚠️ No Python port available~n");

send_map_to_python(Port, MapState) ->
    try
        % Send as binary term like CN system
        MapBinary = term_to_binary(MapState),
        port_command(Port, MapBinary),
        io:format("📤 Enhanced map sent to Python visualizer~n")
    catch
        _:Error ->
            io:format("❌ Error sending to Python: ~p~n", [Error])
    end.