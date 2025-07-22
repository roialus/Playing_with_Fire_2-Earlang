%%%-------------------------------------------------------------------
%%% @author dolev
%%% @copyright (C) 2025, <COMPANY>
%%% @doc Common parameters used that are used in several files.
%%% All reside here for consistency and ease of change
%%%
%%% @end
%%% Created : 11. Jul 2025 23:25
%%%-------------------------------------------------------------------
-author("dolev").

%% Timing related definitions
-define(EXPLODE_DELAY, 3000). % time for normal explosion to occur
-define(FREEZE_DELAY, 2000). % time added to the counter when a bomb is frozen
-define(TICK_DELAY, 50). % a small delay, used for very short bomb interactions and io handler polling intervals
-define(HALFWAY_TILE, 350). % time to complete half the movement - the switching point between tiles
-define(TILE_MOVE, 700). % time to complete a tile movement in normal movespeed (=1)


%%% ================== Naming from map generator ==================
%% todo: There's a copy of them in the map_generator.erl for now. later on it should be removed and also reference this

%% Tile type definitions
-define(FREE, free).
-define(BREAKABLE, breakable).
-define(UNBREAKABLE, unbreakable).
-define(STRONG, strong).
-define(PLAYER_START, player_start).

%% Naming convention for powerups
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

%% Bomb type definitions
-define(NO_BOMB, none).
-define(NORMAL_BOMB, normal_bomb).
-define(REMOTE_BOMB, remote_bomb).
-define(FREEZE_BOMB_ITEM, freeze_bomb_item).

%% Player ID definitions
-define(NO_PLAYER, none).
-define(PLAYER_1, player_1).
-define(PLAYER_2, player_2).
-define(PLAYER_3, player_3).
-define(PLAYER_4, player_4).
