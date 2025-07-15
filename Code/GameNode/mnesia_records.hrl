%%%-------------------------------------------------------------------
%%% @author dolev
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jul 2025 23:29
%%%-------------------------------------------------------------------
-author("dolev").

%% * All record definitions regarding the mnesia database are defined here

-record(gn_state, {
    tiles_table_name,
    bombs_table_name,
    powerups_table_name,
    players_table_name
}).


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

-record(mnesia_players, {
    % identification
    player_number, % 1/2/3/4
    position, % [X,Y]
    direction, % desired direction movement - none/up/down/left/right
    movement, % false |{true,TimerRef}

    % * Exclusive to the mnesia record - not on the player FSM
    hosted = false, % true/false - whether the player is physically playing on this computer (where the GN record is)
    
    % Process info
    % //request_cooldown = 0,   % milliseconds until next GN request allowed
    local_gn = default, % which GN (registered name) does the player FSM & IO is physically running on 
    local_gn_pid = default, % which gn (**PID**) does the player FSM sends all his problems
    target_gn = default, % Which GN (register name) does the player need to communicate with (in whose quarter is he)
    io_handler_pid = default,      % I/O Handler PID
    
    % player FSM pid
    pid = default,

    % Connection status
    disconnected = 0,     % counter to 60 (seconds), then kill process
    bot = false,         % true/false - is this a bot player

    % Stats
    life = 3,
    speed = 1,           % movement speed
    bombs = 1,           % max bombs at the same time
    explosion_radius = 1,
    special_abilities = [], % list of power-ups

    bombs_placed = 0,    % currently placed bombs (bombs - bombs_placed = available)
    immunity_timer = none, % reference to immunity timer
    last_request_time = 0  % timestamp of last GN request
}).