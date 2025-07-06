%% Generated 16x16 Bomberman Map with Unified Grid
-module(test_unified_map).
-export([get_map/0, get_cell_at/2, get_tile_at/2, get_powerup_at/2, get_bomb_at/2, get_player_at/2, get_player_starts/0]).

%% Tile type atoms
-define(FREE, free).
-define(BREAKABLE, breakable).
-define(UNBREAKABLE, unbreakable).
-define(STRONG, strong).
-define(PLAYER_START, player_start).

%% Power-up atoms
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

%% Bomb type atoms
-define(NO_BOMB, none).
-define(NORMAL_BOMB, normal_bomb).
-define(REMOTE_BOMB, remote_bomb).
-define(FREEZE_BOMB_ITEM, freeze_bomb_item).

%% Player ID atoms
-define(NO_PLAYER, none).
-define(PLAYER_1, player_1).
-define(PLAYER_2, player_2).
-define(PLAYER_3, player_3).
-define(PLAYER_4, player_4).

get_map() ->
    [
        [{unbreakable, none, none, none}, {unbreakable, none, none, none}, {unbreakable, none, none, none}, {unbreakable, none, none, none}, {unbreakable, none, none, none}, {unbreakable, none, none, none}, {unbreakable, none, none, none}, {unbreakable, none, none, none}, {unbreakable, none, none, none}, {unbreakable, none, none, none}, {unbreakable, none, none, none}, {unbreakable, none, none, none}, {unbreakable, none, none, none}, {unbreakable, none, none, none}, {unbreakable, none, none, none}, {unbreakable, none, none, none}],
        [{unbreakable, none, none, none}, {player_start, none, none, player_1}, {free, none, none, none}, {free, none, none, none}, {breakable, plus_bombs, none, none}, {free, none, none, none}, {breakable, none, none, none}, {free, none, none, none}, {free, none, none, none}, {free, none, none, none}, {free, none, none, none}, {free, none, none, none}, {free, none, none, none}, {free, none, none, none}, {player_start, none, none, player_2}, {unbreakable, none, none, none}],
        [{unbreakable, none, none, none}, {free, none, none, none}, {free, none, none, none}, {strong, phased, none, none}, {breakable, none, none, none}, {breakable, remote_ignition, none, none}, {unbreakable, none, none, none}, {unbreakable, none, none, none}, {free, none, none, none}, {free, none, none, none}, {strong, kick_bomb, none, none}, {breakable, kick_bomb, none, none}, {free, none, none, none}, {free, none, none, none}, {free, none, none, none}, {unbreakable, none, none, none}],
        [{unbreakable, none, none, none}, {free, none, none, none}, {breakable, remote_ignition, none, none}, {free, none, none, none}, {breakable, move_speed, none, none}, {free, none, none, none}, {free, none, none, none}, {breakable, none, none, none}, {breakable, remote_ignition, none, none}, {unbreakable, none, none, none}, {unbreakable, none, none, none}, {free, none, none, none}, {unbreakable, none, none, none}, {free, none, none, none}, {breakable, none, none, none}, {unbreakable, none, none, none}],
        [{unbreakable, none, none, none}, {free, none, none, none}, {breakable, plus_bombs, none, none}, {free, none, none, none}, {breakable, freeze_bomb, none, none}, {free, none, none, none}, {strong, plus_life, none, none}, {strong, bigger_explosion, none, none}, {free, none, none, none}, {free, none, none, none}, {breakable, kick_bomb, none, none}, {breakable, repeat_bombs, none, none}, {breakable, none, none, none}, {breakable, kick_bomb, none, none}, {free, none, none, none}, {unbreakable, none, none, none}],
        [{unbreakable, none, none, none}, {free, none, none, none}, {strong, phased, none, none}, {free, none, none, none}, {breakable, none, none, none}, {free, none, none, none}, {breakable, none, none, none}, {free, none, none, none}, {breakable, plus_bombs, none, none}, {breakable, kick_bomb, none, none}, {free, none, none, none}, {unbreakable, none, none, none}, {free, none, none, none}, {breakable, repeat_bombs, none, none}, {breakable, repeat_bombs, none, none}, {unbreakable, none, none, none}],
        [{unbreakable, none, none, none}, {free, none, none, none}, {breakable, repeat_bombs, none, none}, {strong, freeze_bomb, none, none}, {breakable, none, none, none}, {free, none, none, none}, {free, none, none, none}, {unbreakable, none, none, none}, {strong, bigger_explosion, none, none}, {free, none, none, none}, {breakable, none, none, none}, {breakable, none, none, none}, {strong, bigger_explosion, none, none}, {free, none, none, none}, {unbreakable, none, none, none}, {unbreakable, none, none, none}],
        [{unbreakable, none, none, none}, {free, none, none, none}, {breakable, kick_bomb, none, none}, {free, none, none, none}, {breakable, none, none, none}, {unbreakable, none, none, none}, {free, none, none, none}, {unbreakable, none, none, none}, {breakable, none, none, none}, {strong, kick_bomb, none, none}, {unbreakable, none, none, none}, {breakable, none, none, none}, {free, none, none, none}, {strong, plus_life, none, none}, {breakable, kick_bomb, none, none}, {unbreakable, none, none, none}],
        [{unbreakable, none, none, none}, {free, none, none, none}, {free, none, none, none}, {breakable, none, none, none}, {breakable, none, none, none}, {free, none, none, none}, {free, none, none, none}, {breakable, plus_bombs, none, none}, {free, none, none, none}, {breakable, remote_ignition, none, none}, {breakable, none, none, none}, {breakable, none, none, none}, {breakable, none, none, none}, {free, none, none, none}, {breakable, none, none, none}, {unbreakable, none, none, none}],
        [{unbreakable, none, none, none}, {free, none, none, none}, {unbreakable, none, none, none}, {free, none, none, none}, {strong, plus_life, none, none}, {free, none, none, none}, {unbreakable, none, none, none}, {free, none, none, none}, {unbreakable, none, none, none}, {free, none, none, none}, {free, none, none, none}, {breakable, none, none, none}, {breakable, remote_ignition, none, none}, {breakable, bigger_explosion, none, none}, {breakable, none, none, none}, {unbreakable, none, none, none}],
        [{unbreakable, none, none, none}, {free, none, none, none}, {unbreakable, none, none, none}, {unbreakable, none, none, none}, {breakable, none, none, none}, {strong, remote_ignition, none, none}, {free, none, none, none}, {breakable, plus_life, none, none}, {breakable, none, none, none}, {free, none, none, none}, {breakable, none, none, none}, {strong, bigger_explosion, none, none}, {free, none, none, none}, {breakable, none, none, none}, {unbreakable, none, none, none}, {unbreakable, none, none, none}],
        [{unbreakable, none, none, none}, {free, none, none, none}, {unbreakable, none, none, none}, {breakable, none, none, none}, {free, none, none, none}, {strong, remote_ignition, none, none}, {breakable, plus_bombs, none, none}, {unbreakable, none, none, none}, {breakable, plus_life, none, none}, {unbreakable, none, none, none}, {free, none, none, none}, {breakable, none, none, none}, {free, none, none, none}, {breakable, plus_bombs, none, none}, {breakable, kick_bomb, none, none}, {unbreakable, none, none, none}],
        [{unbreakable, none, none, none}, {free, none, none, none}, {breakable, plus_bombs, none, none}, {strong, move_speed, none, none}, {free, none, none, none}, {free, none, none, none}, {free, none, none, none}, {breakable, none, none, none}, {free, none, none, none}, {free, none, none, none}, {free, none, none, none}, {strong, phased, none, none}, {free, none, none, none}, {strong, plus_bombs, none, none}, {strong, plus_life, none, none}, {unbreakable, none, none, none}],
        [{unbreakable, none, none, none}, {free, none, none, none}, {free, none, none, none}, {free, none, none, none}, {breakable, plus_bombs, none, none}, {unbreakable, none, none, none}, {free, none, none, none}, {free, none, none, none}, {free, none, none, none}, {free, none, none, none}, {free, none, none, none}, {free, none, none, none}, {strong, move_speed, none, none}, {free, none, none, none}, {free, none, none, none}, {unbreakable, none, none, none}],
        [{unbreakable, none, none, none}, {player_start, none, none, player_3}, {free, none, none, none}, {free, none, none, none}, {free, none, none, none}, {unbreakable, none, none, none}, {free, none, none, none}, {free, none, none, none}, {strong, remote_ignition, none, none}, {free, none, none, none}, {free, none, none, none}, {free, none, none, none}, {free, none, none, none}, {free, none, none, none}, {player_start, none, none, player_4}, {unbreakable, none, none, none}],
        [{unbreakable, none, none, none}, {unbreakable, none, none, none}, {unbreakable, none, none, none}, {unbreakable, none, none, none}, {unbreakable, none, none, none}, {unbreakable, none, none, none}, {unbreakable, none, none, none}, {unbreakable, none, none, none}, {unbreakable, none, none, none}, {unbreakable, none, none, none}, {unbreakable, none, none, none}, {unbreakable, none, none, none}, {unbreakable, none, none, none}, {unbreakable, none, none, none}, {unbreakable, none, none, none}, {unbreakable, none, none, none}]
    ].

get_cell_at(X, Y) when X >= 0, X < 16, Y >= 0, Y < 16 ->
    Map = get_map(),
    Row = lists:nth(X + 1, Map),
    lists:nth(Y + 1, Row).

get_tile_at(X, Y) ->
    {TileType, _, _, _} = get_cell_at(X, Y),
    TileType.

get_powerup_at(X, Y) ->
    {_, PowerupType, _, _} = get_cell_at(X, Y),
    PowerupType.

get_bomb_at(X, Y) ->
    {_, _, BombType, _} = get_cell_at(X, Y),
    BombType.

get_player_at(X, Y) ->
    {_, _, _, PlayerID} = get_cell_at(X, Y),
    PlayerID.

get_player_starts() ->
    [{player_1, 1, 1}, {player_2, 1, 14}, {player_3, 14, 1}, {player_4, 14, 14}].
