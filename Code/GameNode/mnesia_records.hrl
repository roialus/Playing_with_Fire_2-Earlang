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

-record(mnesia_players, {
    name, % placeholder
    other,
    stats,

    pid = none
}).