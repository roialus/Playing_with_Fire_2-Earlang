%%%-------------------------------------------------------------------
%%% @author dolev
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jul 2025 23:25
%%%-------------------------------------------------------------------
-author("dolev").

%% @doc common parameters that are used over several files and need to be consistent

-define(EXPLODE_DELAY, 3000).
-define(FREEZE_DELAY, 2000).
-define(TICK_DELAY, 50).
-define(HALFWAY_TILE, 350). % time to complete half the movement - the switching point between tiles