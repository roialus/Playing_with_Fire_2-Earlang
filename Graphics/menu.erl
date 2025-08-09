-module(menu).
-export([start/0, send/2]).

start() ->
    Port = open_port({spawn, "python3 PWF2_GN_GUI.py"}, [binary, exit_status]),
    send(Port, "show_main_menu"),
    loop(Port).

send(Port, Command) ->
    Port ! {self(), {command, list_to_binary(Command ++ "\n")}}.

loop(Port) ->
    receive
        {Port, {data, Data}} ->
            Message = binary_to_list(Data),
            io:format("GUI replied: ~s~n", [Message]),
            handle_gui_event(Port, string:trim(Message)),
            loop(Port);
        {Port, {exit_status, Status}} ->
            io:format("GUI exited with status ~p~n", [Status])
    end.

handle_gui_event(Port, "play_clicked") ->
    io:format("User clicked Play – checking server connections...~n"),
    send(Port, "show_loading"),
    check_server_connections(Port);
    
handle_gui_event(Port, "exit_clicked") ->
    io:format("User clicked Exit – exiting~n"),
    port_close(Port),
    ok;

handle_gui_event(Port, "retry_clicked") ->
    io:format("Retrying – checking connections again~n"),
    send(Port, "show_loading"),
    check_server_connections(Port);

handle_gui_event(Port, "return_to_menu") ->
    send(Port, "show_main_menu");

handle_gui_event(Port, "play_game_clicked") ->
    io:format("User chose to play the game~n"),
    send(Port, "show_game_setup"),
    timer:sleep(3000), % Simulate game setup time
    start_game(Port);

handle_gui_event(Port, "bot_clicked") ->
    io:format("User chose bot mode~n"),
    send(Port, "show_game_setup"),
    timer:sleep(3000), % Simulate game setup time
    start_game(Port);

handle_gui_event(Port, "choice_timeout") ->
    io:format("Player choice timed out – defaulting to bot~n"),
    send(Port, "show_game_setup"),
    timer:sleep(3000),
    start_game(Port);

handle_gui_event(_, Unknown) ->
    io:format("Unhandled message: ~p~n", [Unknown]).

%% Check CN server and GN connections
check_server_connections(Port) ->
    spawn(fun() -> connection_checker_loop(Port, 0) end).

connection_checker_loop(Port, ElapsedTime) when ElapsedTime >= 10000 ->
    %% After 10 seconds, show error if still not connected
    send(Port, "show_error");

connection_checker_loop(Port, ElapsedTime) ->
    case check_cn_and_gn_status() of
        {connected, 4} ->
            %% All 4 GNs connected, proceed to player choice
            io:format("All servers connected! Moving to player choice~n"),
            send(Port, "show_player_choice"),
            start_choice_timer(Port);
        {connected, ConnectedCount} ->
            %% CN connected but not all GNs
            io:format("Waiting for connections: ~p/4~n", [ConnectedCount]),
            send(Port, lists:flatten(io_lib:format("show_waiting:~p", [ConnectedCount]))),
            timer:sleep(1000),
            connection_checker_loop(Port, ElapsedTime + 1000);
        not_connected ->
            %% CN not connected
            io:format("CN server not connected~n"),
            send(Port, "show_waiting:0"),
            timer:sleep(1000),
            connection_checker_loop(Port, ElapsedTime + 1000)
    end.

%% Check if CN server is running and how many GNs are connected
check_cn_and_gn_status() ->
    try
        case cn_server:get_connection_status() of
            {ok, ConnectedCount} ->
                {connected, ConnectedCount};
            {error, cn_not_running} ->
                not_connected;
            {error, _} ->
                not_connected
        end
    catch
        _:_ ->
            not_connected
    end.

%% Start 60-second timer for player choice
start_choice_timer(Port) ->
    spawn(fun() ->
        timer:sleep(60000), % 60 seconds
        send(Port, "choice_timeout")
    end).

%% Simulate starting the actual game
start_game(Port) ->
    io:format("Starting game...~n"),
    send(Port, "start_game"),
    %% TO DO: Implement actual game logic here
    %% For now, we'll just exit the menu
    timer:sleep(2000),
    port_close(Port).