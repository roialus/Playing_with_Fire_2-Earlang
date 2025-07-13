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
%% ==================== Intended Workflow =======================
%% *** These functions were written after the mnesia database tests were successful,
%% *** and contain the flow of how this module should work like in the pdf design
%% @doc This is the function to be called when starting from CN. Should be started using the IP prefix of the local network (aka '192.168.1.')
start(IP_prefix) ->
    %% Discovers all GNs in the local network
    GN_list = discover_GNs(IP_prefix), % ? Perplexity suggested using 'nodefinder'. Current function is sweeping all options
    register(cn_start, self()), % ! registers this process locally as 'cn_start' so other nodes can communicate directly to it
    %% Awaiting for connections from all GNs
    ok = await_initial_connections(0, []),
    %% * After each connection, the CN replies with an ACK containing the number of nodes connected so far (incl. current one).
    %% * Is able to receive a connection message and also a disconnect message (someone went back to the menu)
    %% * Returns with an 'ok' (for debugging mostly) after 4 GNs in total are connected.

    %% All GNs connected, send message to all nodes to choose play-mode (bot/human),
    %% ! Assumption: the starting process in each GN is registered locally as gn_start
    %% todo: on the *GN side*, send {Pid, playmode, true} if playing as bot or {Pid, playmode, false} if human-player
    lists:foreach(fun(Node) -> {gn_start, Node} ! {choose_playmode, are_you_bot} end, GN_list),

    %% * From this point until the game actually starts, the GNs shouldn't be able to disconnect/crash.
    %% Initialize map and database across all GNs

    Map = map_generator:test_generation(), % creating a new, randomized map
    %% Starting mnesia database
    NodeList = node() ++ GN_list,
    application:set_env(mnesia, dir, "/home/dolev/Documents/mnesia_files"), % ! Change directory based on PC running on, critical for CN
    mnesia:create_schema(NodeList), % mnesia start-up requirement
    rpc:multicall(NodeList, application, start, [mnesia]), % Starts mnesia on all nodes
    %% Insert map into mnesia tables
    %% * This is the "degraded" version for simple testing - One CN node and one GN node.
    TableNamesList = lists:map(fun(X) ->
        Result = create_tables(lists:nth(2, NodeList), node(), X),
        io:format("*Create table result: ~p~n", [Result]) 
        end, lists:seq(1,4)),

    %% * Full-fledged version - one CN, 4 GNs
    %%TableNamesList = lists:map(fun(X) ->
    %%        create_tables(lists:nth(X, NodeList), node(), X)
    %%    end, lists:seq(1,length(NodeList))),
    
    %% * The loading of the mnesia tables is done in parallel 
    Mnesia_loading_pid = spawn_link(?MODULE, initial_mnesia_load, [TableNamesList, Map]),
    %% Await GNs decision - play as bot or human
    GNs_decisions = await_players_decisions(4,[], GN_list),

    %% TODO: This is stupid way to check if the mnesia load finished, need to think about something better, maybe message when its done?
    case erlang:is_process_alive(Mnesia_loading_pid) of
        false -> ok; % finished initializing by the time all GNs sent their answer
        true -> 
            timer:sleep(5000),
            false = erlang:is_process_alive(Mnesia_loading_pid) % ? crash the process if it's still alive
    end,
    {ok, _Pid_cn_graphics} = cn_graphics_server:start_link(), % TODO: module doesn't exist.
    %% TODO: on init, the graphics server spawns(%likns) all gn graphics servers
    {ok, _Pid_cn_server} = cn_server:start_link(GNs_decisions), % TODO: module doesn't exist.
    %% TODO: On init, the cn server spawns(&links) all gn gen_servers.
    ok.


%% =================== Auxiliary Functions ======================

%% @doc Collects GNs connection requests, exits when 4 nodes are conected. Monitors whoever connects for process exit,
%% Replies with {connect_ack, Count} where Count is the number of nodes connected so far (incl. current).
%% Able to receive: {Pid, connect} , {Pid, disconnect}, {'DOWN', Ref, process, Pid, _Reason}
%% When done returns 'ok'.
await_initial_connections(4, _List) -> ok;
await_initial_connections(Acc,List) ->
    %% * List = [{Pid, Ref}, {Pid, Ref}, ... ]
    receive
        {From, connect} -> 
            Ref = erlang:monitor(process, From), % monitor incoming process for unexpect failure
            NewCount = Acc + 1,
            From ! {conect_ack, NewCount},
            await_initial_connections(NewCount, [{From, Ref} | List]);
        {From, disconnect} -> % remove that process from the list, reduce counter
            {value, {_,Ref_disconnected}, NewList} = lists:keytake(From, 1, List),
            erlang:demonitor(Ref_disconnected), % demonitor process
            await_initial_connections(Acc-1, NewList);
        {'DOWN', Ref, process, Pid, _Reason} = Msg ->  % connected process closed unexpectedly
            case lists:keytake(Ref, 2, List) of
                false -> % caught a 'DOWN' from someone else, re-queue it in mailbox
                    self() ! Msg; 
                {value, {Pid, Ref}, NewList} -> % process exists within our list
                    await_initial_connections(Acc-1, NewList)
            end
    end.


%% @doc awaits mnesia table's finalized setup, then inserts the generated map-state to the tables
initial_mnesia_load(TableNamesList, Map) ->
    mnesia:wait_for_tables(lists:flatten(TableNamesList), 5000), % ? timeout is 5000ms for now
    insert_map_to_database(Map),
    io:format("*Initial map state loaded successfully to mnesia tables~n").


%% @doc recieve-block that catches all decisions from GNs and returns a sorted list of tuples
await_players_decisions(0, Acc, _GN_list) -> lists:sort(fun({A,_}, {B,_}) -> A =< B end, Acc);
await_players_decisions(N, Acc, GN_list) ->
    receive
        {Pid, playmode, Answer} when is_pid(Pid), is_boolean(Answer) ->
            case lists:member(node(Pid),GN_list) of
                true ->
                    GN_number = list_to_integer([lists:nth(3, atom_to_list(node(Pid)))]),
                    await_players_decisions(N-1, [{GN_number, Answer} | Acc ], GN_list);
                false -> % caught a different message, re-queue in mailbox
                    self() ! {Pid, Answer},
                    await_players_decisions(N, Acc, GN_list)
            end
    end.




%% @doc Attempting to communicate with all possible IPs in the local network, returning a the GNs in the network (nodes called GNx@192.168.1.Y )
discover_GNs(IP_prefix) ->
    Looking_for = ["GN1@", "GN2@", "GN3@", "GN4@"],
    lists:flatten(
        [
            case net_adm:names(IP_prefix ++ integer_to_list(X)) of % asks each IP for all nodes he operates
                {ok, Names} -> % IP responded with a lists of his erlang nodes
                    %% filter only nodes of our GNs
                    [NodeName || {Name, _Port} <- Names, NodeNameStr = Name ++ "@" ++ "Prefix" ++ integer_to_list(X), % reconstruct the node name
                lists:any(fun(Y) -> lists:prefix(Y, NodeNameStr) end, Looking_for),
                NodeName = list_to_atom(NodeNameStr),
                NodeName =/= node(), % not my own node
                net_adm:ping(NodeName) =:= pong % ping node
            ];
            _ -> [] % IP didn't respond, ignore it (probably doesn't hold our GNs)
            end || X <- lists:seq(3,253)]
        ).




%% ==================== Testing Functions =======================
%% todo: for super-massive debugging use sys:trace(Pid, true).
%% @doc connecting nodes according to an IP list, returning nodelist used for test/1
connecting_nodes(IPList) ->
    %% each erl shell will be called 'GN#@IP where IP is from the list.
    IPsAsAtoms = lists:map(
        fun(X) -> list_to_atom("GN" ++ integer_to_list(X) ++"@" ++ lists:nth(X, IPList)) end,
        lists:seq(1,length(IPList))),
    lists:foreach(
        fun(IP) -> io:format("Pinging ~w : ~w~n",[IP, net_adm:ping(IP)]) end, IPsAsAtoms),
    [node()] ++ IPsAsAtoms.


-spec test(list()) -> ok.
test(NodeList) ->
    %% NodeList = [node(), gn_node1, gn_node2, gn_node3, gn_node4] -- THIS IS HOW THIS LIST SHOULD LOOK LIKE
    Map = map_generator:test_generation(), % creating a new map from scratch
    %rpc:multicall(NodeList, application, set_env, [mnesia, dir, "/home/dolev/Documents/mnesia_files"]),
    application:set_env(mnesia, dir, "/home/dolev/Documents/mnesia_files"),
    mnesia:create_schema(NodeList),
    rpc:multicall(NodeList, application, start, [mnesia]), % multiple nodes
    % application:start(mnesia), % single node

    %% initialize mnesia tables per each game-node
    %% * This is the "degraded" version for simple testing - One CN node and one GN node.
    TableNamesList = lists:map(fun(X) ->
        Result = create_tables(lists:nth(2, NodeList), node(), X),
        io:format("*Create table result: ~p~n", [Result]) 
        end, lists:seq(1,4)),

    %% * Full-fledged version - one CN, 4 GNs
    %%TableNamesList = lists:map(fun(X) ->
    %%        create_tables(lists:nth(X, NodeList), node(), X)
    %%    end, lists:seq(1,length(NodeList))),

    mnesia:wait_for_tables(lists:flatten(TableNamesList), 5000), % timeout is 5000ms for now

    % ?List all tables
    mnesia:system_info(tables),

    % ? Check if gn3_tiles is loaded and available
    io:format("Tables: ~p~n", [mnesia:system_info(tables)]),

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


%% ==============================================================


%% helper function to create mnesia table names
generate_atom_table_names(Number, Type) ->
    list_to_atom("gn" ++ integer_to_list(Number) ++ Type).

create_tables(GN_node, CN_node, Node_number) ->
    Mnesia_tiles_name = generate_atom_table_names(Node_number, "_tiles"),
    Mnesia_bombs_name = generate_atom_table_names(Node_number, "_bombs"),
    Mnesia_powerups_name = generate_atom_table_names(Node_number, "_powerups"),
    Mnesia_players_name = generate_atom_table_names(Node_number, "_players"),
    %% initialize all mnesia tables, per game node
    Debug1 = mnesia:create_table(Mnesia_tiles_name, [
        {attributes, record_info(fields, mnesia_tiles)},
        {disc_copies, [CN_node]},
        {ram_copies, [GN_node]},
        {record_name, mnesia_tiles},
        {type, set}
        ]),
    Debug2 = mnesia:create_table(Mnesia_bombs_name, [
        {attributes, record_info(fields, mnesia_bombs)},
        {disc_copies, [CN_node]},
        {ram_copies, [GN_node]},
        {record_name, mnesia_bombs},
        {type, set}
    ]),
    Debug3 = mnesia:create_table(Mnesia_powerups_name, [
        {attributes, record_info(fields, mnesia_powerups)},
        {disc_copies, [CN_node]},
        {ram_copies, [GN_node]},
        {record_name, mnesia_powerups},
        {type, set}
    ]),
    Debug4 = mnesia:create_table(Mnesia_players_name, [
        {attributes, record_info(fields, mnesia_players)},
        {disc_copies, [CN_node]},
        {ram_copies, [GN_node]},
        {record_name, mnesia_players},
        {type, set}
    ]),
    io:format("CN: full printout of create_tables for node number #~w:~ntiles: ~w~nbombs: ~w~npowerups: ~w~nplayers: ~w~n", 
        [Node_number,Debug1 ,Debug2, Debug3, Debug4]).


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

