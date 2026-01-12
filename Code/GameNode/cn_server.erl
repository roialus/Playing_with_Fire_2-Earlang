%%%-------------------------------------------------------------------
%%% @author dolev
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% No longer starts the gn_servers. 
%%% Instead links to them in a similar manner to the graphics setup
%%% @end
%%% Created : 13. Jul 2025 23:41
%%%-------------------------------------------------------------------
-module(cn_server).
-author("dolev").

-behaviour(gen_server).

% API
-export([start_link/1]).
-import(gn_server, [generate_atom_table_names/2]). % to not have to specify the import everytime

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-include_lib("mnesia_records.hrl").
%% Linux compatible
%-include_lib("src/clean-repo/Code/common_parameters.hrl").
%-include_lib("src/clean-repo/Code/Objects/object_records.hrl").
%% Windows compatible - Fixed relative paths
-include("../Objects/object_records.hrl").
-include("../common_parameters.hrl").

% Required module for QLC
-include_lib("stdlib/include/qlc.hrl").

-record(gn_data, {
    pid,
    tiles,
    bombs,
    powerups,
    players
    }).

%%%===================================================================
%%% API
%%%===================================================================
%% @doc Initialization of cn_server. *Registers globally* as "cn_server", maximal priority on CN node
start_link(GN_playmode_list) ->
    % * GN_playmode_list = [{1, true}, {2, false}, ... ]
    gen_server:start_link({global, ?MODULE}, ?MODULE, [GN_playmode_list], [{priority, max}]). 

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% @doc Initializes all 4 gn servers, assuring proper creation ({Ok, Pid}) and monitoring them (timeout after 20 sec as backup),
%% The data stored is a list of records, each record contains the names (atoms) of the mnesia tables the CN shares with him
%% Accessing the name can be in 2 ways:
%% "Nameless": lists:nth(2, CN_data)#gn_data.players OR
%% "named" (records in the list are named, [Gn1_names, Gn2_names, ...] ): Gn2_names#gn_data.players
init([GN_playmode_list]) -> % [ {GN_number, Answer, NodeID} , {..} ]
    process_flag(trap_exit, true), % set to trap exits of GNs
    %% Attempt to monitor the processes right after initialization
    erlang:send_after(0, self(), {monitor_GNs, GN_playmode_list}),

    CN_data = none, %! initialize WITHOUT any data
    {ok, CN_data}.

%%%================== handle call ==================
%% @doc 
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%%%================== handle cast ==================

%% @doc Handling forwarding requests
handle_cast({forward_request, Destination, Request}, State) ->
    %% * forward requests look like {forward_request, Destination_GN_name, Request={..} }
    %% * Sends the new message as {forwarded, Request={..}}
    io:format("🔄 CN SERVER: Forwarding request to ~p: ~p~n", [Destination, Request]),
    gn_server:cast_message(Destination, {forwarded, Request}),
    {noreply, State};

%% @doc Handling checks to switch GNs
handle_cast({query_request, AskingGN, Request}, State) ->
    io:format("🔄 CN SERVER: Received query_request from ~p: ~p~n", [AskingGN, Request]),
    %% * this below is the Request's contents
    case Request of
        {move_request_out_of_bounds, player, PlayerNum, [X,Y]=Destination_coord, Direction} ->
        %% * finds which GN oversees the coordinate, extract from Player's mnesia table the relevant buffs
        %% * pass the appropriate GN the message:
        %% * {forwarded, {move_request_out_of_bounds, player, {playerNum, Destination_coord, Direction, [relevant buffs], AskingGN}
            TargetGN = req_player_move:get_managing_node_by_coord(X, Y),
            
            %% Get the asking GN's data to read the player record (player belongs to asking GN, not target GN)
            AskingGNIndex = req_player_move:node_name_to_number(AskingGN),
            AskingGNData = lists:nth(AskingGNIndex, State),
            Players_table = AskingGNData#gn_data.players,
            Player_record = req_player_move:read_player_from_table(PlayerNum, Players_table),
            case erlang:is_record(Player_record, mnesia_players) of
                true -> 
                    io:format("🔄 CN SERVER: Forwarding player ~p move request from ~p to ~p at coordinates ~p~n", [PlayerNum, AskingGN, TargetGN, Destination_coord]),
                    gn_server:cast_message(TargetGN,
                        {move_request_out_of_bounds, player,
                            {PlayerNum, Destination_coord, Direction, Player_record#mnesia_players.special_abilities, AskingGN}
                    });   
                false ->
                    io:format("❌ CN SERVER: Player ~p not found in asking GN ~p table, Player_record: ~p~n", [PlayerNum, AskingGN, Player_record]),
                    erlang:error(record_not_found, [node(), Player_record])
            end,
            {noreply, State};

        {handle_bomb_explosion, Coord, Radius} ->
            %% handled in a side function
            %% Calculates affected coordinates, then sends damage_taken messages to all objects impacted
            bomb_explosion_handler(Coord, Radius),
            {noreply, State};

        {ignite_bomb_request, PlayerNum} ->
            case bomb_helper_functions:find_remote_bombs_for_player(player_fsm:get_player_pid(PlayerNum)) of
                RemoteBombs when is_list(RemoteBombs) ->
                    lists:foreach(fun(BombRecord) ->
                        % Send ignite message to each remote bomb
                        case BombRecord#mnesia_bombs.pid of
                            Pid when is_pid(Pid) ->
                                bomb_as_fsm:ignite_bomb(Pid);
                            _ ->
                                io:format("Warning: Invalid bomb PID for remote bomb at ~p~n", [BombRecord#mnesia_bombs.position])
                        end
                    end, RemoteBombs),
                    %% Notify Player FSM about the ignition
                    case RemoteBombs of
                        [] -> player_fsm:gn_response(PlayerNum, {ignite_result, denied});
                        _  -> player_fsm:gn_response(PlayerNum, {ignite_result, accepted})
                    end;
                {aborted, Reason} ->
                    io:format("❌ Failed to find remote bombs for player ~p: ~p~n", [PlayerNum, Reason]),
                    player_fsm:gn_response(PlayerNum, {ignite_result, denied});
                Other ->
                    io:format("❌ Unexpected result finding remote bombs for player ~p: ~p~n", [PlayerNum, Other]),
                    player_fsm:gn_response(PlayerNum, {ignite_result, denied})
            end,
            {noreply, State}
    end;

%% * handles a player transfer from one GN to another
handle_cast({transfer_records, player, PlayerNum, Current_GN, New_GN}, State) ->
    Current_GNIndex = req_player_move:node_name_to_number(Current_GN),
    New_GNIndex = req_player_move:node_name_to_number(New_GN),
    Current_GNData = lists:nth(Current_GNIndex, State),
    New_GNData = lists:nth(New_GNIndex, State),
    Current_GN_players_table = Current_GNData#gn_data.players,
    New_GN_players_table = New_GNData#gn_data.players,
    case transfer_player_records(PlayerNum, Current_GN_players_table, New_GN_players_table) of
        ok -> 
            %% Message the new GN to check for collisions
            gn_server:cast_message(New_GN,{incoming_player, PlayerNum}),
            {noreply, State};
        {error, not_found} ->
            io:format("❌ CN_SERVER: transfer_player_records - player ~p not found in table ~p~n", [PlayerNum, Current_GN_players_table]),
            {noreply, State};
        {error, Reason} ->
            io:format("❌ CN_SERVER: transfer_player_records failed for player ~p: ~p~n", [PlayerNum, Reason]),
            {noreply, State}
    end;

%% * Update active bombs in mnesia table and notify controlling player_fsm
handle_cast({player_bomb_exploded, PlayerPid}, State) ->
    update_player_active_bombs(PlayerPid),
    {noreply, State};


%% @doc General cast messages - as of now ignored.
handle_cast(_Msg, State) ->
    {noreply, State}.


%%%================== handle info ==================
%% @doc Initialization of GN_data - sets up the links to all gn_servers
handle_info({monitor_GNs, GN_playmode_list}, _IrreleventState) ->
    GN_pids_list = link_GNs_loop(lists:seq(1, length(GN_playmode_list))),
    CN_data = lists:map(
        fun(Index) -> 
            Individual_table_names = generate_table_names(Index),
            PidA = lists:nth(Index, GN_pids_list),
            [TilesTable, BombsTable, PowerupsTable, PlayersTable] = Individual_table_names,
            #gn_data{
                pid = PidA,
                tiles = TilesTable,
                bombs = BombsTable,
                powerups = PowerupsTable,
                players = PlayersTable
            }
        end, lists:seq(1,4)),
    io:format("Successfully linked to all gn_servers ~w~n", [GN_pids_list]),
    %% Store the CN data in the state
    {noreply, CN_data};

%% @doc Receiving ready message from cn_server_graphics
handle_info({graphics_ready, _GraphicsPid}, State) ->
    io:format("**CN_SERVER: Graphics server is ready~n"),
    %% Notify all GN servers to start the game
    lists:foreach(fun(#gn_data{pid = Pid}) ->
        io:format("**CN_SERVER: Sending start_game to GN server: ~p~n", [Pid]),
        Pid ! start_game
    end, State),
    {noreply, State};

%% @doc Handles failure messages from the monitored processes
handle_info({'EXIT', Pid, Reason} , Data=[_GN1=#gn_data{}, _GN2=#gn_data{}, _GN3=#gn_data{}, _GN4=#gn_data{}]) -> 
    io:format("*CN: Linked process ~w failed, reason:~w~n",[Pid, Reason]),
    NewData = handle_gn_crash(Pid, Data),
    {noreply, NewData};

%% @doc General messages received as info - as of now ignored.
handle_info(_Info, State) ->
    io:format("**CN_SERVER: Received Unhandled info message:~w~n", [_Info]),
    {noreply, State}.


%% @doc 
terminate(_Reason, _State) ->
    ok.

%% @doc 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
generate_table_names(GN) ->
    [generate_atom_table_names(GN, "_tiles"), generate_atom_table_names(GN, "_bombs"),
        generate_atom_table_names(GN, "_powerups"), generate_atom_table_names(GN, "_players")].



%% ? I used this in earlier iteration, but changed the code where it was needed. Remove this comment if its used after-all
%% find_pid_by_node(TargetNode, GNList) ->
%%     case lists:filter(
%%         fun(#gn_data{pid = Pid}) -> node(Pid) =:= TargetNode end, GNList) of
%%         
%%         [#gn_data{pid = Pid}] -> Pid;
%%         _ -> pid_not_found
%%     end.


%% @doc Transfers a player's mnesia table from one GN to another
transfer_player_records(PlayerNum, Current_GN_table, New_GN_table) ->
    Fun = fun() ->
        %% Read entry from current GN
        case mnesia:read(Current_GN_table, PlayerNum, read) of
            [Record] ->
                %% delete from table from the GN we are leaving
                ok = mnesia:delete(Current_GN_table, PlayerNum, write),
                %% Write the data to the new GN's table
                mnesia:write(New_GN_table, Record, write),
                ok;
            [] ->
                {error, not_found}
        end
    end,
    case mnesia:activity(transaction, Fun) of
        ok -> ok;
        {atomic, ok} -> ok;
        {atomic, {error, not_found}} -> {error, not_found};
        {aborted, Reason} -> {error, Reason};
        Other -> {error, Other}
    end.

bomb_explosion_handler(Coord, Radius) ->
    ResultList = case calculate_explosion_reach(Coord, Radius) of
        {atomic, Result} -> 
            io:format("💥 EXPLOSION_HANDLER: Got atomic result with ~p coordinates~n", [length(lists:flatten(Result))]),
            Result;
        Result when is_list(Result) -> 
            io:format("💥 EXPLOSION_HANDLER: Got list result with coordinates:~p~n", [Result]),
            Result;
        Other -> 
            io:format("ERROR: Unexpected result from calculate_explosion_reach: ~p~n", [Other]),
            throw({unexpected_explosion_result, Other})
    end,
    %% * ResultList looks like [ Coordinates from GN1, Coordinates From GN2, .. Gn3, GN4 ] where each one is [X,Y], [X,Y], [X,Y] with duplicates on origin coordinate
    %% ResultList can be passed to the graphics server so it knows where to show an explosion
    FlattenedCoords = [X || X <- lists:usort(ResultList), X =/= []],
    io:format("💥 EXPLOSION_HANDLER: Sending ~p coordinates to graphics: ~p~n", [length(FlattenedCoords), FlattenedCoords]),
    cn_server_graphics:show_explosion(FlattenedCoords),
    %% Sends inflict_damage messages to all objects affected by the explosion
    notify_affected_objects(ResultList).

calculate_explosion_reach([X, Y], Max_range) ->
    Fun = fun() -> calculate_affected([X, Y], Max_range) end,
	mnesia:activity(transaction, Fun).


-spec calculate_affected(Center::list(), Radius::integer()) -> list().
calculate_affected([X,Y] = Center, Radius) ->
	North = {0, 1}, South = {0, -1}, East = {1, 0}, West = {-1, 0},
	
	EmptyResults = {[], [], [], []},
	
	{CenterIndex, _} = req_player_move:get_gn_number_by_coord(X,Y),
	IntermedResults = erlang:setelement(CenterIndex, EmptyResults, [Center]),
	
	NorthResults = trace_ray(Center, North, Radius, IntermedResults),
	SouthResults = trace_ray(Center, South, Radius, IntermedResults),
	EastResults = trace_ray(Center, East, Radius, IntermedResults),
	WestResults = trace_ray(Center, West, Radius, IntermedResults),
	
	merge_results(NorthResults, SouthResults, EastResults, WestResults).
	
	
merge_results({List1a, List1b, List1c, List1d}, {List2a, List2b, List2c, List2d}, 
            {List3a, List3b, List3c, List3d}, {List4a, List4b, List4c, List4d}) ->
	[List1a ++ List2a ++ List3a ++ List4a, 
    List1b ++ List2b ++ List3b ++ List4b, 
    List1c ++ List2c ++ List3c ++ List4c, 
    List1d ++ List2d ++ List3d ++ List4d].
	

%% end of recursion - reverse the lists.
trace_ray(_CurCoord, _Direction, 0, {List1, List2, List3, List4}) -> 
	{lists:reverse(List1), lists:reverse(List2), lists:reverse(List3), lists:reverse(List4)};

trace_ray([X, Y], {PlusX, PlusY}=Direction, StepsLeft, Accums) ->
	[NextX, NextY] = [X + PlusX, Y + PlusY],
	{TableIndex, TableName} = req_player_move:get_gn_number_by_coord(NextX, NextY),
	
	case mnesia:read(TableName, [NextX, NextY]) of
		[] -> % no tile found
			UpdatedList = erlang:element(TableIndex, Accums),
			NewAccums = erlang:setelement(TableIndex, Accums, [ [NextX, NextY] | UpdatedList]),
			trace_ray([NextX, NextY], Direction, StepsLeft-1, NewAccums);
		[_] -> % found a tile
			UpdatedList = erlang:element(TableIndex, Accums),
			NewAccums = erlang:setelement(TableIndex, Accums, [ [NextX, NextY] | UpdatedList]),
			trace_ray([NextX, NextY], Direction, 0, NewAccums) % go to the end of the recursion
	end.

%% Handler for letting all objects be affected by the explosion in the affected coordinates list
notify_affected_objects(ResultList) ->
    %% Deduplicate coordinates for each GN to prevent multiple damage application
    GN1_coords = lists:usort(lists:nth(1, ResultList)),
    GN2_coords = lists:usort(lists:nth(2, ResultList)),
    GN3_coords = lists:usort(lists:nth(3, ResultList)),
    GN4_coords = lists:usort(lists:nth(4, ResultList)),
    
    io:format("💥 NOTIFY_AFFECTED: GN1: ~p, GN2: ~p, GN3: ~p, GN4: ~p~n", 
              [GN1_coords, GN2_coords, GN3_coords, GN4_coords]),
    
    spawn(fun() -> process_affected_objects(GN1_coords, gn1_tiles, gn1_bombs, gn1_players) end),
    spawn(fun() -> process_affected_objects(GN2_coords, gn2_tiles, gn2_bombs, gn2_players) end),
    spawn(fun() -> process_affected_objects(GN3_coords, gn3_tiles, gn3_bombs, gn3_players) end),
    spawn(fun() -> process_affected_objects(GN4_coords, gn4_tiles, gn4_bombs, gn4_players) end).

process_affected_objects(ListOfCoords, Tiles_table, Bombs_table, Players_table) ->
    Fun = fun() -> lists:foreach(
        fun(Coord) -> process_single_coord(Coord, Tiles_table, Bombs_table, Players_table) end, ListOfCoords
    ) end,
    MnesiaResult = mnesia:activity(transaction, Fun),
    io:format("DEBUG: Mnesia return value is ~p~n", [MnesiaResult]),
    case MnesiaResult of
        ok -> ok;
        {atomic, Result} -> Result;
        {aborted, Reason} -> 
            io:format("❌ Mnesia transaction aborted for coord ~p: ~p~n", [ListOfCoords, Reason]),
            ok;
        Other -> 
            io:format("❌ Unexpected result from mnesia activity for coord ~p: ~p~n", [ListOfCoords, Other]),
            ok
    end.

process_single_coord(Coord, Tiles_table, Bombs_table, Players_table) ->
    %% using QLC querries to make this faster
    TilesRecords = qlc:e(qlc:q(
        [T#mnesia_tiles{} || T <- mnesia:table(Tiles_table), T#mnesia_tiles.position == Coord]
    )),
    BombsRecords = qlc:e(qlc:q(
        [B#mnesia_bombs{} || B <- mnesia:table(Bombs_table), B#mnesia_bombs.position == Coord]
    )),
    PlayersRecords = qlc:e(qlc:q(
        [P#mnesia_players{} || P <- mnesia:table(Players_table), P#mnesia_players.position == Coord]
    )),
    %% Send 'inflict damage' message to all affected objects, based on their type (bomb/player/tile)
    %% io print of the affected objects
    io:format("💥 EXPLOSION at ~p affects ~p tiles, ~p bombs, and ~p players.~n",
        [Coord, length(TilesRecords), length(BombsRecords), length(PlayersRecords)]),
    inflict_damage_handler(TilesRecords, tile, damage_taken, Tiles_table),
    inflict_damage_handler(BombsRecords, bomb_as_fsm, damage_taken, Bombs_table),
    inflict_damage_handler(PlayersRecords, player_fsm, inflict_damage, Players_table),
    ok.


inflict_damage_handler(RecordsList, Module, Function, Table) ->
    lists:foreach(fun(Record) ->
        try
            case Record of
                R when is_record(R, mnesia_tiles) ->
                    io:format("💥 Tile at ~p taking damage~n", [Record#mnesia_tiles.position]),
                    _ = apply(Module, Function, [Record#mnesia_tiles.pid]),
                    io:format("💥 Sent damage to: ~p:~p(~p)~n", [Module, Function, Record#mnesia_tiles.pid]);
                R when is_record(R, mnesia_bombs) ->
                    io:format("💥 Bomb at ~p taking damage~n", [Record#mnesia_bombs.position]),
                    _ = apply(Module, Function, [Record#mnesia_bombs.pid]),
                    io:format("💥 Sent damage to: ~p:~p(~p)~n", [Module, Function, Record#mnesia_bombs.pid]);
                R when is_record(R, mnesia_players) ->
                    io:format("💥 Player ~p at ~p taking damage~n", [Record#mnesia_players.player_number, Record#mnesia_players.position]),
                    _ = apply(Module, Function, [Record#mnesia_players.pid]),
                    io:format("💥 Sent damage to: ~p:~p(~p)~n", [Module, Function, Record#mnesia_players.pid]),
                    UpdatedRecord = Record#mnesia_players{life = Record#mnesia_players.life - 1},
                    mnesia:dirty_write(Table, UpdatedRecord),
                    io:format("💥 Updated player ~p life to ~p~n", [Record#mnesia_players.player_number, UpdatedRecord#mnesia_players.life])
            end
        catch
            Class:Reason ->
                io:format(standard_error, "Error calling ~p:~p(). Class: ~p, Reason: ~p~n", [Module, Function, Class, Reason])
        end
    end, RecordsList),
    ok.


update_player_active_bombs(PlayerNumber) ->
    Tables = [gn1_players, gn2_players, gn3_players, gn4_players],
    Fun = fun() -> find_in_player_tables_by_number(PlayerNumber, Tables) end,
    Result = case mnesia:activity(transaction, Fun) of
        {atomic, R} -> R;
        R -> R
    end,
    case Result of
        not_found ->
            erlang:error(player_not_found, [PlayerNumber]);
        {table_updated, Player_record} ->
            %% Notify player_fsm of this change directly
            player_fsm:bomb_exploded(Player_record#mnesia_players.pid)
    end.


find_in_player_tables_by_number(_PlayerNumber, []) -> not_found;
find_in_player_tables_by_number(PlayerNumber, [Table| T]) ->
    case mnesia:index_read(Table, PlayerNumber, player_number) of
        [Record] ->  % update active bombs in mnesia table
            UpdatedRecord = Record#mnesia_players{bombs_placed = Record#mnesia_players.bombs_placed - 1},
            mnesia:write(Table, UpdatedRecord, write),
            {table_updated, Record};
        [] -> find_in_player_tables_by_number(PlayerNumber, T)
    end.


%% Functions used to link the game nodes
link_GNs_loop(NodeNumbers) ->
    io:format("Attempt to link to all gn_servers..~n"),
    Pids = lists:map(fun(NodeNum) ->
       GN_server_name = list_to_atom("GN" ++ integer_to_list(NodeNum) ++ "_server"),
       link_with_retry(GN_server_name, 0)
    end, NodeNumbers),
    Pids. % return list of linked PIDs

link_with_retry(GN_server_name, RetryCount) when RetryCount > 20 ->
    erlang:error({link_failed_after_retries, GN_server_name, RetryCount});
link_with_retry(GN_server_name, RetryCount) ->
    Pid = global:whereis_name(GN_server_name),
    case Pid of
        undefined ->
            io:format("Process ~p not found globally, attempt ~w/20, retrying...~n", [GN_server_name, RetryCount + 1]),
            timer:sleep(500),
            link_with_retry(GN_server_name, RetryCount + 1);
        _ ->
            try
                link(Pid),
                io:format("Successfully linked to ~p~n", [GN_server_name]),
                Pid
            catch
                _:_ ->
                    io:format("Failed to link to ~p, attempt ~w/20, retrying...~n", [GN_server_name, RetryCount + 1]),
                    timer:sleep(500),
                    link_with_retry(GN_server_name, RetryCount + 1)
            end
    end.

%% === Node crash handling functions ===
%% @doc Handle game node crashes
handle_gn_crash(Pid, Data=[GN1=#gn_data{}, GN2=#gn_data{}, GN3=#gn_data{}, GN4=#gn_data{}]) ->
    %% find which GN crashed
    PidsList = [GN1#gn_data.pid, GN2#gn_data.pid, GN3#gn_data.pid, GN4#gn_data.pid],
    case lists:filter(fun(X) -> X == Pid end, PidsList) of
        [] -> 
            io:format("**CN_SERVER: Non gn_server process crashed. Pid: ~p~n", [Pid]),
            Data; % do nothing
        [CrashedPid] -> 
            io:format("**CN_SERVER: Known gn_server process crashed. Pid: ~p~n", [CrashedPid]),
            %% Retreives which GN it was - 1-4
            Zip = lists:zip(PidsList, lists:seq(1,4)),
            case lists:keyfind(CrashedPid, 1, Zip) of
                false ->
                    io:format("**CN_SERVER: Could not locate crashed pid in list~n"),
                    Data;
                {_, CrashedGNNum} ->
                    io:format("**CN_SERVER: Crashed GN: ~p~n", [CrashedGNNum]),
                    %% decide which node will take on this GN's responsibilities - node with least current PIDs
                    NewHostingNode = find_node_with_least_pids(lists:delete(CrashedPid, PidsList)),
                    io:format("**CN_SERVER: New hosting node for crashed GN #~p (Pid - ~p) is GN ~p~n", [CrashedGNNum, CrashedPid, NewHostingNode]),
                    Recovered_GN_Pid = restart_gn(NewHostingNode, lists:nth(CrashedGNNum, Data), CrashedGNNum),
                    %% replace the pid in the data list
                    NewData = lists:map(
                        fun(#gn_data{pid = OldPid} = GNData) ->
                            case OldPid of
                                CrashedPid -> GNData#gn_data{pid = Recovered_GN_Pid};
                                _ -> GNData
                            end
                        end, Data),
                        io:format("**CN_SERVER: Game node ~p crashed and was successfully recovered~n", [Pid]),
                        NewData
            end;
        MoreThanOne ->
            io:format("**CN_SERVER: ERROR - More than one GN matched crashed Pid: ~p~n", [MoreThanOne]),
            Data % return unchanged
    end.


find_node_with_least_pids(Pids) ->
    NodeCounts = count_nodes(Pids, #{}),
    MinCountNode = find_min(maps:to_list(NodeCounts)),
    element(1, MinCountNode).

count_nodes([], Counts) ->
    Counts;
count_nodes([Pid | T], Counts) ->
    Node = node(Pid),
    NewCounts = maps:update_with(Node, fun(C) -> C + 1 end, 1, Counts),
    count_nodes(T, NewCounts).

find_min(NodeCounts) ->
    Sorted = lists:sort(fun({_NodeA, CountA}, {_NodeB, CountB}) -> CountA < CountB end, NodeCounts),
    hd(Sorted).

%% helper: perform add_table_copy but normalize/catch results so we don't crash CN
safe_add_table_copy(Tab, Node) ->
    Resp = catch mnesia:add_table_copy(Tab, Node, ram_copies),
    case Resp of
        ok -> ok;
        {atomic, ok} -> ok;
        {aborted, Reason} ->
            io:format("**CN_SERVER: add_table_copy aborted for ~p -> ~p: ~p~n", [Tab, Node, Reason]),
            {error, Reason};
        {'EXIT', Reason} ->
            io:format("**CN_SERVER: add_table_copy exception for ~p -> ~p: ~p~n", [Tab, Node, Reason]),
            {error, Reason};
        Other ->
            io:format("**CN_SERVER: add_table_copy unexpected result for ~p -> ~p: ~p~n", [Tab, Node, Other]),
            {error, Other}
    end.

%% Moves the mnesia table to the new node, restarts the GN processes on that node
restart_gn(Node, CrashedGN_gndata=#gn_data{}, CrashedGNNum) -> 
    %% copy the mnesia tables of the crashed GN to the new node
    Tables = [CrashedGN_gndata#gn_data.tiles, CrashedGN_gndata#gn_data.powerups,
              CrashedGN_gndata#gn_data.players, CrashedGN_gndata#gn_data.bombs],
    Results = [ safe_add_table_copy(Tab, Node) || Tab <- Tables ],
    case lists:all(fun(X) -> X == ok end, Results) of
        true ->
            io:format("**CN_SERVER: RECOVERY - Transferred mnesia tables to node ~p~n", [Node]);
        false ->
            %% log and continue restart (won't crash CN).
            io:format("**CN_SERVER: WARNING - add_table_copy had failures for node ~p: ~p. Continuing restart without full copies.~n",
                      [Node, Results])
    end,
    %% *** Start the GN process on the new node ***
    %% This will also initialize the bot_handler and player_fsm processes
    %% And also initialize all TILES based on the data within the mnesia table, over-writing their existing pids
    io:format("**CN_SERVER: RECOVERY - Restarting GN #~p on node ~p..~n", [CrashedGNNum, Node]),
    NewGN_Pid = case rpc:call(Node, gn_server, start, [{CrashedGNNum, true, true}]) of
        {badarg, Reason} ->
            io:format("**CN_SERVER: RECOVERY - Failed to start GN #~p on node ~p: ~p~n", [CrashedGNNum, Node, Reason]),
            erlang:error(gn_recovery_failed, [CrashedGNNum, Node, Reason]);
        {ok, Pid} -> Pid;
        {error, Reason} ->
            io:format("**CN_SERVER: RECOVERY - Failed to start GN #~p on node ~p: ~p~n", [CrashedGNNum, Node, Reason]),
            erlang:error(gn_recovery_failed, [CrashedGNNum, Node, Reason]);
        Pid when is_pid(Pid) -> Pid;
        Other ->
            io:format("**CN_SERVER: RECOVERY - Unexpected result starting GN #~p on node ~p: ~p~n", [CrashedGNNum, Node, Other]),
            erlang:error(gn_recovery_failed, [CrashedGNNum, Node, Other])
    end,
    io:format("**CN_SERVER: RECOVERY - Linking to new GN #~p with PID ~p~n", [CrashedGNNum, NewGN_Pid]),
    link(NewGN_Pid),
    io:format("**CN_SERVER: RECOVERY - Restarted GN #~p on node ~p with PID ~p~n", [CrashedGNNum, Node, NewGN_Pid]),
    io:format("**CN_SERVER: RECOVERY - Setting up power-ups, players and bombs in GN~p~n", [CrashedGNNum]),
    io:format("**CN_SERVER: RECOVERY - finished recovering all objects for GN #~p~n", [CrashedGNNum]),
    NewGN_Pid.
