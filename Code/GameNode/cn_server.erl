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
-import(gn_server, [generate_atom_table_names/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-include_lib("mnesia_records.hrl").
-include_lib("src/Playing_with_Fire_2-Earlang/Code/Objects/object_records.hrl").


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
%% @doc Initializes all 4 gn servers, assuring proper creation ({Ok, Pid}) (timeout after 20 sec as backup),
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


    


%% @doc 
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%% @doc 
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc 
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