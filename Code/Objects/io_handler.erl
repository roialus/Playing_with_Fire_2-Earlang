%%%-------------------------------------------------------------------
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% I/O Handler for Player FSM
%%% Flow: I/O -> Player -> GN -> CN -> GN -> Player -> I/O
%%% @end
%%% Created : 06. Jul 2025
%%%-------------------------------------------------------------------
-module(io_handler).

-behaviour(gen_server).

%% API
-export([start_link/2, send_input/2, set_player_pid/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

% linux compatible
-include_lib("src/clean-repo/Code/common_parameters.hrl").


-record(io_state, {
    player_pid = none,              % Player FSM PID
    player_number,           % Player number (1-4)
    waiting_for_ack = false, % Waiting for player response
    input_buffer = [],       % Buffered inputs while waiting
    keyboard_mode = true     % true for keyboard, false for bot
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start I/O handler for a player
start_link(PlayerNumber, KeyboardMode) ->
    ServerName = list_to_atom("io_handler_" ++ integer_to_list(PlayerNumber)),
    gen_server:start_link({local, ServerName}, ?MODULE, 
        [PlayerNumber, KeyboardMode], []).

%% @doc Send input (for testing)
send_input(PlayerNumber, Input) ->
    ServerName = list_to_atom("io_handler_" ++ integer_to_list(PlayerNumber)),
    gen_server:cast(ServerName, {external_input, Input}).

%% @doc Set the player PID after player FSM starts
set_player_pid(IOHandlerPid, PlayerPid) ->
    gen_server:call(IOHandlerPid, {set_player_pid, PlayerPid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([PlayerNumber, KeyboardMode]) ->
    State = #io_state{
        player_number = PlayerNumber,
        keyboard_mode = KeyboardMode
    },
    
    % Start input polling if in keyboard mode
    case KeyboardMode of
        true -> erlang:send_after(?TICK_DELAY, self(), poll_input);
        false -> ok
    end,
    
    {ok, State}.

handle_call({set_player_pid, PlayerPid}, _From, State) ->
    NewState = State#io_state{player_pid = PlayerPid},
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, unknown_request, State}.

handle_cast({external_input, Input}, State) ->
    process_input(Input, State);

handle_cast({player_ack, Response}, State) ->
    % Player acknowledged our command
    NewState = State#io_state{waiting_for_ack = false},
    
    % Display response to user
    display_response(Response, State#io_state.player_number),
    
    % Process any buffered inputs
    case NewState#io_state.input_buffer of
        [] -> 
            {noreply, NewState};
        [NextInput | Rest] ->
            UpdatedState = NewState#io_state{input_buffer = Rest},
            process_input(NextInput, UpdatedState)
    end;

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(poll_input, State) ->
    % Poll for keyboard input
    Input = read_keyboard_input(),
    
    NewState = case Input of
        no_input -> State;
        Key -> 
            case process_input(Key, State) of
                {noreply, S} -> S;
                _ -> State
            end
    end,
    
    % Schedule next poll
    erlang:send_after(?TICK_DELAY, self(), poll_input),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

process_input(Input, State) ->
    case convert_input_to_command(Input) of
        {ok, Command} ->
            case State#io_state.waiting_for_ack of
                true ->
                    % Buffer input while waiting for ack
                    NewBuffer = State#io_state.input_buffer ++ [Input],
                    NewState = State#io_state{input_buffer = NewBuffer},
                    {noreply, NewState};
                false ->
                    % Send command to player FSM
                    case State#io_state.player_pid of
                        undefined ->
                            io:format("Player ~p: No player process connected~n", 
                                [State#io_state.player_number]),
                            {noreply, State};
                        PlayerPid ->
                            player_fsm:input_command(PlayerPid, Command),
                            NewState = State#io_state{waiting_for_ack = true},
                            {noreply, NewState}
                    end
            end;
        invalid_input ->
            {noreply, State}
    end.

convert_input_to_command(Input) ->
    case Input of
        % Movement commands
        w -> {ok, {move, up}};
        s -> {ok, {move, down}};
        a -> {ok, {move, left}};
        d -> {ok, {move, right}};
        
        up -> {ok, {move, up}};
        down -> {ok, {move, down}};
        left -> {ok, {move, left}};
        right -> {ok, {move, right}};
        
        % Arrow key commands
        arrow_up -> {ok, {move, up}};
        arrow_down -> {ok, {move, down}};
        arrow_left -> {ok, {move, left}};
        arrow_right -> {ok, {move, right}};
        
        % Bomb commands
        space -> {ok, drop_bomb};
        e -> {ok, drop_bomb};   % Drop bomb on space or 'e'
        
        % Remote bomb ignition
        q -> {ok, ignite_remote};   % 'q' for remote bomb ignition
        
        % Control commands
        escape -> {ok, quit};  % Escape to quit
        
        _ -> invalid_input
    end.

%% @doc Display the response from the player FSM
display_response(Response, PlayerNumber) ->
    case Response of
        {move_success, NewPos, none} ->
            io:format("Player ~p: Moved to ~p~n", [PlayerNumber, NewPos]);
        {move_success, NewPos, Powerup} ->
            io:format("Player ~p: Moved to ~p and picked up ~p!~n", 
                [PlayerNumber, NewPos, Powerup]);
        {move_failed, Reason} ->
            io:format("Player ~p: Move failed - ~p~n", [PlayerNumber, Reason]);
        bomb_dropped ->
            io:format("Player ~p: Bomb dropped!~n", [PlayerNumber]);
        {bomb_failed, Reason} ->
            io:format("Player ~p: Bomb drop failed - ~p~n", [PlayerNumber, Reason]);
        {ignited_bombs, Count} ->
            io:format("Player ~p: Ignited ~p remote bombs!~n", [PlayerNumber, Count]);
        {damage_taken, NewLife} ->
            io:format("Player ~p: Took damage! Lives remaining: ~p~n", [PlayerNumber, NewLife]);
        player_died ->
            io:format("Player ~p: DIED!~n", [PlayerNumber]);
        player_dead ->
            io:format("Player ~p: You are dead!~n", [PlayerNumber]);
        {error, cooldown} ->
            io:format("Player ~p: Please wait before next action~n", [PlayerNumber]);
        {error, Reason} ->
            io:format("Player ~p: Error - ~p~n", [PlayerNumber, Reason]);
        _ ->
            io:format("Player ~p: ~p~n", [PlayerNumber, Response])
    end.

%% @doc Read keyboard input 
read_keyboard_input() ->
    % For testing
    case io:get_chars('', 1) of
        eof -> no_input;
        " " -> space;
        "w" -> w;
        "a" -> a;
        "s" -> s;
        "d" -> d;
        "e" -> b;
        "q" -> q;
        "\e" -> escape;
        _ -> no_input
    end.
