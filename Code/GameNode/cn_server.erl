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

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-include_lib("mnesia_records.hrl").
-include_lib("src/Playing_with_Fire_2-Earlang/Code/Objects/object_records.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc Initialization of 
start_link(GN_playmode_list) ->
    % * GN_playmode_list = [{1, true}, {2, false}, ... ]
    %% registered *globally* as "cn_server", maximal priority on CN node
    gen_server:start_link({global, ?MODULE}, ?MODULE, [GN_playmode_list], [{priority, max}]). 

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% @doc 
%% TODO: initialize the gn_server processes(linking).
%% todo: 1. Think what data should be held here (record of all tables? sorted how? idk)
%% todo: 2. Keep return values (Pids) of the GNs? verify proper creation against {ok, _Pid} ?
%% todo: 3. Finish the lists:map
init([GN_playmode_list]) ->
    process_flag(trap_exit, true), % set to trap exits of GNs
    _Return_map = lists:map(fun(X) -> X end, GN_playmode_list),
    {ok, no_data}.

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
