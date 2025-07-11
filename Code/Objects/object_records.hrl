%%%-------------------------------------------------------------------
%%% @author dolev
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jul 2025 23:23
%%%-------------------------------------------------------------------
-author("dolev").

%% @doc All records are defined in one place. A separate file exists for common -define()

-record(powerup_state, {
    position, % position - [X,Y]
    type, % type of power up - can be movement speed, extra bombs etc..
    original_node_ID % original creating node ID - TODO: unsure of necessity
}).

-record(tile_state, {
    position, % position - [X,Y]
    type, % can be - unbreakable, breakable, two_hit (-> one_hit)
    contains % can be - none (no power-up), bomb (that'll trigger on its own), or any speed-up
}).

-record(bomb_state, {
    type, % type of bomb - regular/remote/repeating(W.I.P - not yet implemented)
    ignited = false, % bomb ignition status - can be false or {true, TimerRef}
    status = normal, % normal/frozen
    radius = 1, % radius of explosion - number means how many tiles in each direction in + shape
    position, % location of bomb, [X,Y]
    direction = none, % desired moving direction, can be - none/up/down/left/right
    movement = false, % can be - false / {true, TimerRef}
    owner, % owner of the bomb - can be none or player ID (not pid)
    gn_pid % GN pid who oversees this process
}).