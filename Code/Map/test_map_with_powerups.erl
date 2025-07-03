%% Generated 16x16 Bomberman Map with Power-ups
-module(test_map_with_powerups).
-export([get_map/0, get_powerup_map/0, get_tile_type/2, get_powerup_at/2, get_player_starts/0]).

-define(FREE, 0).
-define(BREAKABLE, 1).
-define(UNBREAKABLE, 2).
-define(STRONG, 3).
-define(PLAYER_START, 4).

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

get_map() ->
    [
        [2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2],
        [2, 4, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 2],
        [2, 0, 0, 0, 1, 1, 0, 1, 0, 3, 1, 0, 3, 0, 0, 2],
        [2, 0, 1, 2, 1, 2, 2, 3, 1, 1, 0, 3, 1, 0, 0, 2],
        [2, 2, 1, 3, 0, 1, 3, 1, 3, 3, 2, 0, 1, 1, 0, 2],
        [2, 2, 0, 1, 1, 1, 1, 1, 1, 0, 1, 3, 0, 0, 0, 2],
        [2, 0, 0, 0, 2, 3, 1, 0, 1, 3, 3, 1, 2, 0, 0, 2],
        [2, 1, 0, 0, 2, 0, 1, 1, 1, 1, 3, 1, 1, 1, 0, 2],
        [2, 0, 3, 0, 0, 1, 1, 1, 1, 2, 3, 0, 0, 0, 0, 2],
        [2, 0, 3, 0, 1, 1, 2, 1, 0, 1, 3, 1, 0, 0, 0, 2],
        [2, 2, 1, 2, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2],
        [2, 0, 1, 2, 2, 0, 1, 1, 1, 2, 0, 3, 0, 0, 1, 2],
        [2, 3, 1, 2, 0, 3, 3, 0, 2, 1, 0, 1, 0, 1, 1, 2],
        [2, 0, 0, 1, 1, 0, 0, 0, 0, 1, 3, 2, 0, 0, 0, 2],
        [2, 4, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 4, 2],
        [2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2]
    ].

get_powerup_map() ->
    [
        [none, none, none, none, none, none, none, none, none, none, none, none, none, none, none, none],
        [none, none, none, plus_life, none, none, none, none, none, none, none, none, none, none, none, none],
        [none, none, none, none, freeze_bomb, none, none, plus_bombs, none, move_speed, plus_life, none, phased, none, none, none],
        [none, none, none, none, plus_life, none, none, plus_life, none, none, none, bigger_explosion, none, none, none, none],
        [none, none, none, repeat_bombs, none, none, bigger_explosion, none, remote_ignition, bigger_explosion, none, none, bigger_explosion, none, none, none],
        [none, none, none, kick_bomb, remote_ignition, none, none, plus_bombs, none, none, none, remote_ignition, none, none, none, none],
        [none, none, none, none, none, bigger_explosion, none, none, plus_life, move_speed, plus_bombs, kick_bomb, none, none, none, none],
        [none, plus_life, none, none, none, none, none, kick_bomb, plus_bombs, plus_life, kick_bomb, move_speed, remote_ignition, freeze_bomb, none, none],
        [none, none, plus_life, none, none, repeat_bombs, none, plus_bombs, none, none, plus_bombs, none, none, none, none, none],
        [none, none, move_speed, none, none, plus_life, none, move_speed, none, none, kick_bomb, none, none, none, none, none],
        [none, none, remote_ignition, none, none, repeat_bombs, none, none, none, none, none, none, none, none, none, none],
        [none, none, none, none, none, none, bigger_explosion, none, plus_bombs, none, none, bigger_explosion, none, none, plus_life, none],
        [none, remote_ignition, remote_ignition, none, none, bigger_explosion, freeze_bomb, none, none, none, none, plus_life, none, none, none, none],
        [none, none, none, none, bigger_explosion, none, none, none, none, remote_ignition, plus_life, none, none, none, none, none],
        [none, none, none, none, none, none, none, kick_bomb, none, phased, remote_ignition, none, none, none, none, none],
        [none, none, none, none, none, none, none, none, none, none, none, none, none, none, none, none]
    ].

get_tile_type(X, Y) when X >= 0, X < 16, Y >= 0, Y < 16 ->
    Map = get_map(),
    Row = lists:nth(X + 1, Map),
    lists:nth(Y + 1, Row).

get_powerup_at(X, Y) when X >= 0, X < 16, Y >= 0, Y < 16 ->
    PowerupMap = get_powerup_map(),
    Row = lists:nth(X + 1, PowerupMap),
    lists:nth(Y + 1, Row).

get_player_starts() ->
    [{player_1, 1, 1}, {player_2, 1, 14}, {player_3, 14, 1}, {player_4, 14, 14}].
