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


%% Naming convention for powerups
%% Power-up definitions (atoms)
-define(NO_POWERUP, none).
-define(MOVE_SPEED, move_speed).
-define(REMOTE_IGNITION, remote_ignition).
-define(REPEAT_BOMBS, repeat_bombs).
-define(KICK_BOMB, kick_bomb).
-define(PHASED, phased).
-define(PLUS_BOMBS, plus_bombs).
-define(BIGGER_EXPLOSION, bigger_explosion).
-define(PLUS_LIFE, plus_life).
-define(FREEZE_BOMB, freeze_bomb).

%% Bomb type definitions (atoms)
-define(NO_BOMB, none).
-define(NORMAL_BOMB, normal_bomb).
-define(REMOTE_BOMB, remote_bomb).
-define(FREEZE_BOMB_ITEM, freeze_bomb_item).