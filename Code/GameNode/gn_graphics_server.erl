-module(gn_graphics_server).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    cn_node,                    % Central node
    python_port,                % Port to local Python visualizer
    update_counter = 0          % Update counter
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
init([GNNode]) ->
    io:format("🎮 GN Graphics Server starting on ~w~n", [node()]),
    
    State = #state{cn_node = CNNode},
    
    % Create Python port
    erlang:send_after(25, self(), create_python_port),
    
    io:format("✅ GN Graphics Server initialized~n"),
    {ok, State}.

%% Handle synchronous calls
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% Handle asynchronous casts
handle_cast({map_update, MapState}, State) ->
    % Received map update from CN graphics server - send to Python
    io:format("🗺️ GN received map update, sending to Python~n"),
    send_map_to_python(State#state.python_port, MapState),
    
    NewState = State#state{update_counter = State#state.update_counter + 1},
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Handle messages
handle_info(create_python_port, State) ->
    % Create Python port for visualizer
    io:format("🐍 Creating Python visualizer port...~n"),
    Port = create_python_port(),
    {noreply, State#state{python_port = Port}};

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
    io:format("🛑 GN Graphics Server terminating~n"),
    if State#state.python_port =/= undefined ->
        port_close(State#state.python_port);
    true -> ok
    end,
    ok.

%% Handle code changes
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Python Port Communication
%%%===================================================================

%% Create Python port for visualizer
create_python_port() ->
    try
        Port = open_port({spawn, "python3 gn_map_live.py"}, 
                        [binary, exit_status, {packet, 4}]),
        io:format("✅ Python visualizer port created~n"),
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
        MapBinary = list_to_binary(io_lib:format("~p", [MapState])),
        Port ! {self(), {command, MapBinary}},
        io:format("📤 Map sent to Python visualizer~n")
    catch
        _:Error ->
            io:format("❌ Error sending to Python: ~p~n", [Error])
    end.