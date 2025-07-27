%%%-------------------------------------------------------------------
%%% @author dolev
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
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
%% Windows compatible
-include_lib("project_env/src/Playing_with_Fire_2-Earlang/Code/Objects/object_records.hrl").
-include_lib("project_env/src/Playing_with_Fire_2-Earlang/Code/common_parameters.hrl").


%% todo: move this record (if it is even necessary) to the .hrl
-record(gn_data, {
    pid,
    ref,
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
    GN_pids_list = lists:map(fun({Number, Answer, NodeID}) -> 
        {ok, Pid} = rpc:call(NodeID, gn_server, start_link, [{Number, Answer}], 20000),
        %% now monitor the GN from the CN_server
        Ref = erlang:monitor(process, Pid),
        {Pid, Ref} % return the Pid and Ref
        end, GN_playmode_list), %* there's a timeout of 20 seconds if the connection fails
    CN_data = lists:map(
        fun(Index) -> 
            Individual_table_names = generate_table_names(Index),
            {PidA, RefA} = lists:nth(Index, GN_pids_list),
            #gn_data{
                pid = PidA,
                ref = RefA,
                tiles = lists:hd(Individual_table_names),
                bombs = lists:nth(2, Individual_table_names),
                powerups = lists:nth(3, Individual_table_names),
                players = lists:last(Individual_table_names)
            }
        end, lists:seq(1,4)),
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
    gen_server:cast(Destination, {forwarded, Request}),
    {noreply, State};

%% @doc Handling checks to switch GNs
handle_cast({query_request, AskingGN, Request}, State) ->
    %% * this below is the Request's contents
    case Request of
        {move_request_out_of_bounds, player, PlayerNum, [X,Y]=Destination_coord, Direction} ->
        %% * finds which GN oversees the coordinate, extract from Player's mnesia table the relevant buffs
        %% * pass the appropriate GN the message:
        %% * {forwarded, {move_request_out_of_bounds, player, {playerNum, Destination_coord, Direction, [relevant buffs], AskingGN}
            TargetGN = req_player_move:get_managing_node_by_coord(X, Y),
            Players_table = lists:nth(req_player_move:node_name_to_number(TargetGN), State#gn_data.players),
            Player_record = req_player_move:read_player_from_table(PlayerNum, Players_table),
            case erlang:is_record(Player_record, mnesia_players) of
                true -> 
                    gen_server:cast(TargetGN,
                        {move_request_out_of_bounds, player,
                            {PlayerNum, Destination_coord, Direction, Player_record#mnesia_players.special_abilities, AskingGN}
                    });   
                false ->
                    erlang:error(record_not_found, [node(), Player_record])
            end,
            {noreply, State}
    end;

%% * handles a player transfer from one GN to another
handle_cast({transfer_records, player, PlayerNum, Current_GN, New_GN}, State) ->
    Current_GN_players_table = lists:nth(req_player_move:node_name_to_number(Current_GN), State#gn_data.players),
    New_GN_players_table = lists:nth(req_player_move:node_name_to_number(New_GN), State#gn_data.players),
    case transfer_player_records(PlayerNum, Current_GN_players_table, New_GN_players_table) of
        {error, not_found} -> erlang:error(transfer_player_failed, [node(), PlayerNum]);
        ok -> 
            %% Message the new GN to check for collisions
            gen_server:cast(New_GN,{incoming_player, PlayerNum})
    end,
    {noreply, State};



%% @doc General cast messages - as of now ignored.
handle_cast(_Msg, State) ->
    {noreply, State}.



                


%%%================== handle info ==================

%% @doc Handles failure messages from the monitored processes
handle_info({'DOWN', Ref, process, Pid, Reason} , Data=[GN1=#gn_data{}, GN2=#gn_data{}, GN3=#gn_data{}, GN4=#gn_data{}]) -> 
    %% todo: placeholder
    io:format("*CN: monitored process ~w with ref ~w failed, reason:~w~n",[Pid,Ref,Reason]),
    {noreply, Data};

%% @doc General messages received as info - as of now ignored.
handle_info(_Info, State) ->
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
find_pid_by_node(TargetNode, GNList) ->
    case lists:filter(
        fun(#gn_data{pid = Pid}) -> node(Pid) =:= TargetNode end, GNList) of
        
        [#gn_data{pid = Pid}] -> Pid;
        _ -> pid_not_found
    end.


%% @doc Transfers a player's mnesia table from one GN to another
transfer_player_records(PlayerNum, Current_GN_table, New_GN_table) ->
    Fun = fun() ->
        %% Read entry from current GN
        case mnesia:read(Current_GN_table, PlayerNum, read) of
          [Record] ->
                %% delete from table from the GN we are leaving
                ok = mnesia:delete(Current_GN_table, Record, write),
                %% Write the data to the new GN's table
                mnesia:write(New_GN_table, Record, write);
        [] ->
            {error, not_found}
        end
    end,
    mnesia:activity(transaction, Fun).