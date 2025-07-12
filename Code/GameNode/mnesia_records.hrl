%%%-------------------------------------------------------------------
%%% @author dolev
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jul 2025 23:29
%%%-------------------------------------------------------------------
-author("dolev").

%% @doc All record definitions regarding the mnesia database are defined here

-record(mnesia_tiles, {
    position, % position - [X,Y]
    type, % can be - unbreakable, breakable, two_hit (-> one_hit)
    contains,  % can be - none (no power-up), bomb (that'll trigger on its own), or any speed-up

    pid = none
}).

-record(mnesia_bombs, {
    position, % position - [X,Y]
    type, % type of bomb - regular / remote / repeating
    ignited = false, % if not ignited - holds 'false', if ignited - holds a ref to the timer behind the self msg
    status = normal, % can be - normal / frozen
    radius = 1, % blast radius on a + shape - number is how many blocks away the explosion is felt
    movement = false, % false / {true, TimerRef}
    owner = none, % player name/ID (?) of whoever placed the bomb. 'none' is for a bomb that fell from a broken tile (or simply no owner)
    gn_pid, % GN Pid who oversees this process

    pid = none
}).

-record(mnesia_powerups, {
    position, % position - [X,Y]
    type, % type of power up - can be movement speed, extra bombs etc..
    original_node_ID, % original creating node ID - TODO: unsure of necessity

    pid = none
}).

-record(mnesia_players, { %% @doc this is a mish-mash between existing player_fsm.erl and what's needed
    % identification
    player_ID, % player_1/player_2 ..
    position, % [X,Y]
    next_position, % [X', Y'] - intended next position % todo: this has to change

    % Process info
    request_cooldown = 0,  % milliseconds until next GN request allowed
    original_node_id,      % node where player was created
    %process_id,           % this process PID - CALLED 'pid' below
    gn_pid,              % GN PID
    io_handler_pid,      % I/O Handler PID

    % Connection status
    disconnected = 0,     % counter to 60 (seconds), then kill process
    bot = false,         % true/false - is this a bot player

    % Default stats
    life = 3,
    speed = 1,           % movement speed
    bombs = 1,           % max bombs at the same time
    explosion_radius = 1,
    special_abilities = [], % list of power-ups

    % Dynamic state
    bombs_placed = 0,    % currently placed bombs (bombs - bombs_placed = available)
    immunity_timer = none, % reference to immunity timer
    last_request_time = 0,  % timestamp of last GN request


    pid = none
}).