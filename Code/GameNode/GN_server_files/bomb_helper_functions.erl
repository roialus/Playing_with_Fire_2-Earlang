-module(bomb_helper_functions).

-include("../../common_parameters.hrl").
-include("../mnesia_records.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([place_bomb/3, find_remote_bombs_for_player/1, update_bomb_timer/3]).

%% @doc Places a bomb for a player at their current position
%% @param PlayerNum - The player number/ID placing the bomb
%% @param PlayersTableName - The name of the mnesia table containing player data
%% @returns bomb_placed | {error, Reason}
place_bomb(PlayerNum, PlayersTableName, BombsTableName) ->
    %% Look up player in mnesia table
    Fun = fun() ->
        case mnesia:read(PlayersTableName, PlayerNum, read) of
            [PlayerRecord] -> 
                {ok, PlayerRecord};
            [] -> {error, player_not_found};
            _Multiple -> {error, multiple_players_found}
        end
    end,
    {atomic, Result} = mnesia:transaction(Fun),

    case Result of
        {ok, PlayerRecord} ->
            %% Get player position and bomb settings
            [X,Y] = PlayerRecord#mnesia_players.position,
            BombType = get_bomb_type(PlayerRecord#mnesia_players.special_abilities),
            BombRadius = PlayerRecord#mnesia_players.explosion_radius,
            
            %% Create bomb process using FSM
            case bomb_as_fsm:start_monitor(X, Y, BombType, [PlayerRecord#mnesia_players.pid, BombRadius]) of
                {ok, {BombPid, _MonitorRef}} ->
                    %% Update player's active bomb within mnesia
                    case update_player_bomb_count(PlayerNum, PlayersTableName, 1) of
                        {atomic, _} -> 
                            %% Generate bomb record for table
                            BombRecord = #mnesia_bombs{
                                position = [X,Y],
                                type = BombType,
                                radius = BombRadius,
                                owner = PlayerRecord#mnesia_players.pid,
                                gn_pid = PlayerRecord#mnesia_players.target_gn,
                                pid = BombPid},
                            io:format("DEBUG: Created bomb record and adding to table: ~p~n", [BombsTableName]),
                            case add_bomb_to_table(BombRecord, BombsTableName) of
                                {atomic, _} -> bomb_placed;
                                ok -> bomb_placed;
                                Error2 -> 
                                    io:format("ERROR: add_bomb_to_table failed: ~p~n", [Error2]),
                                    {error, {failed_to_add_bomb_to_table, Error2}}
                            end;
                        ok -> 
                            %% Generate bomb record for table
                            BombRecord = #mnesia_bombs{
                                position = [X,Y],
                                type = BombType,
                                radius = BombRadius,
                                owner = PlayerRecord#mnesia_players.pid,
                                gn_pid = PlayerRecord#mnesia_players.target_gn,
                                pid = BombPid},
                            io:format("DEBUG: Created bomb record and adding to table: ~p~n", [BombsTableName]),
                            case add_bomb_to_table(BombRecord, BombsTableName) of
                                {atomic, _} -> bomb_placed;
                                ok -> bomb_placed;
                                Error2 -> 
                                    io:format("ERROR: add_bomb_to_table failed: ~p~n", [Error2]),
                                    {error, {failed_to_add_bomb_to_table, Error2}}
                            end;
                        Error1 -> 
                            io:format("ERROR: update_player_bomb_count failed: ~p~n", [Error1]),
                            {error, {failed_to_update_bomb_count, Error1}}
                    end;
                Error ->
                    {error, {failed_to_create_bomb, Error}}
            end;

        {error, player_not_found} -> {error, player_not_found};
        {error, multiple_players_found} -> {error, multiple_players_found}
    end.

%% Helper function to determine bomb type based on player buffs
get_bomb_type(BuffList) ->
    case lists:member(?REMOTE_IGNITION, BuffList) of
        true -> ?REMOTE_IGNITION;
        false ->
            case lists:member(?REPEAT_BOMBS, BuffList) of
                true -> ?REPEAT_BOMBS;
                false -> ?NORMAL_BOMB
            end
    end.


%% Helper function to update player's active bomb count 
update_player_bomb_count(PlayerNum, PlayersTableName, Increment) ->
    Fun = fun() ->
        [PlayerRecord] = mnesia:read(PlayersTableName, PlayerNum, write),
        UpdatedRecord = PlayerRecord#mnesia_players{
            bombs_placed = PlayerRecord#mnesia_players.bombs_placed + Increment
        },
        mnesia:write(PlayersTableName, UpdatedRecord, write)
    end,
    mnesia:activity(transaction, Fun).

%% Helper function to add bomb to bombs mnesia table
add_bomb_to_table(Record, BombsTableName) ->
    Fun = fun() ->
        mnesia:write(BombsTableName, Record, write)
    end,
    mnesia:activity(transaction, Fun).

%% @doc Searches all 4 bomb tables for not-ignited remote bombs owned by PlayerNum
%% @returns List of remote bomb records owned by the player
find_remote_bombs_for_player(PlayerPid) ->
    BombTables = [gn1_bombs, gn2_bombs, gn3_bombs, gn4_bombs],
    Fun = fun() ->
        lists:foldl(
            fun(TableName, Acc) ->
                Query = qlc:q([
                    Bomb || Bomb <- mnesia:table(TableName),
                    Bomb#mnesia_bombs.owner =:= PlayerPid,
                    Bomb#mnesia_bombs.type =:= ?REMOTE_IGNITION,
                    Bomb#mnesia_bombs.ignited =:= false
                ]),
                qlc:e(Query) ++ Acc
            end,
            [], % Initial accumulator
            BombTables)
    end,
    RemoteBombs = case mnesia:activity(transaction, Fun) of
        {atomic, R} -> R;
        R -> R
    end,
    RemoteBombs.

update_bomb_timer(BombPid, NewTime, BombsTableName) ->
    Fun = fun() ->
        %% Find bomb by pid
        Query = qlc:q([Bomb || Bomb <- mnesia:table(BombsTableName), Bomb#mnesia_bombs.pid =:= BombPid]),
        case qlc:e(Query) of
            [BombRecord] ->
                %% Update internal_timer field
                UpdatedRecord = BombRecord#mnesia_bombs{
                    internal_timer = NewTime
                },
                mnesia:write(BombsTableName, UpdatedRecord, write),
                ok;
            [] -> {error, bomb_not_found};
            _Multiple -> {error, multiple_bombs_found}
        end
    end,
    mnesia:activity(transaction, Fun).