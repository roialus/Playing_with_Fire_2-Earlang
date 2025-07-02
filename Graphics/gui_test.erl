-module(gui_test).
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
    io:format("User clicked Play – switching to loading...~n"),
    send(Port, "show_loading"),
    timer:sleep(10000),
    send(Port, "show_error");
    
handle_gui_event(Port, "exit_clicked") ->
    io:format("User clicked Exit – exiting~n"),
    port_close(Port),
    ok;

handle_gui_event(Port, "retry_clicked") ->
    io:format("Retrying – back to loading screen~n"),
    send(Port, "show_loading"),
    timer:sleep(5000),
    send(Port, "show_error");

handle_gui_event(Port, "return_to_menu") ->
    send(Port, "show_main_menu");

handle_gui_event(_, Unknown) ->
    io:format("Unhandled message: ~p~n", [Unknown]).
