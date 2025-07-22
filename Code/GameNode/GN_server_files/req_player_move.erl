%%%-------------------------------------------------------------------
%%% @author dolev
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Jul 2025 08:42
%%%-------------------------------------------------------------------

-module(req_player_move).
-author(dolev).

%% ? Because the gn_server file became insanely cluttered, I'm splitting it into different files
%% ? based on functionality. This file will include all functions relevant to a player requesting movement
%% * it might also include a bomb requesting movement later on
-export[read_player_from_table/2, calc_new_coordinates/2,
        update_player_direction/3, handle_player_movement_clearance/3, handle_bomb_movement_clearance/3].


-export[get_managing_node_by_coord/2, node_name_to_number/1].
-export[get_records_at_location/2, interact_with_entity/4].
-export[handle_player_movement/3, insert_player_movement/2, check_for_obstacles/4].
-export[read_and_update_coord/3].
-export[check_entered_coord/2].

-import(gn_server, [get_registered_name/1]).


%%% ===========================================================================
%% ? Imports for windows:
%-include_lib("project_env/src/Playing_with_Fire_2-Earlang/Code/Objects/common_parameters.hrl").
%-include_lib("project_env/src/Playing_with_Fire_2-Earlang/Code/GameNode/mnesia_records.hrl").
%-include_lib("project_env/src/Playing_with_Fire_2-Earlang/Code/Objects/object_records.hrl"). %% windows fix

%% ? imports for linux:
-include_lib("src/clean-repo/Code/Objects/object_records.hrl"). %% This should work for compiling under rebar3.
-include_lib("src/clean-repo/Code/GameNode/mnesia_records.hrl").
-include_lib("src/clean-repo/Code/common_parameters.hrl").
%%% ===========================================================================

%% @doc Updates coordinate after a movement timer expires.
%% If new coord. is within the same GN - update position, direction and movement
%%      then check for collisions in the new coordinate. (*not in this function)
%% else - Send message to player FSM to update his targetGN, update position, direction and movement in record, 
%%      and request CN to tranfer the player entry to the new GN's table, who at last asks the new GN to check for collisions (*not in this function)
read_and_update_coord(player, PlayerNum, Table) ->
    Fun = fun() ->
        case mnesia:read(Table, PlayerNum, write) of
            [Player_record = #mnesia_players{}] -> 
                [New_x, New_y] = calc_new_coordinates(Player_record, Player_record#mnesia_players.direction),
                Current_gn_name = get_registered_name(self()),

                %% check if new coordinate fall within current managing GN
                case get_managing_node_by_coord(New_x,New_y) of
                    Current_gn_name -> % destination coordinate is managed by this GN
                        %% update position, reset direction and movement
                        Updated_record = Player_record#mnesia_players{
                            position = [New_x, New_y],
                            movement = false,
                            direction = none
                        },
                        %% ? should we check for collisions at this point? against explosions?
                        mnesia:write(Updated_record),
                        {same_gn, Player_record}; %% return value to calling function
                    Other_name -> %% destination coordinate is managed by another GN (=Other_name)
                    %% update position, target_gn name, reset movement and direction
                    %% ask CN to transfer entry between tables
                        Updated_record = Player_record#mnesia_players{
                            position = [New_x, New_y],
                            target_gn = Other_name,
                            movement = false,
                            direction = none
                        },
                        mnesia:write(Updated_record),
                        {switch_gn, Player_record, Current_gn_name, Other_name} %% return value
                    end;
            [] -> not_found % should cause an error
        end
    end,
    mnesia:activity(transaction, Fun).


-spec read_player_from_table(PlayerNum::integer(), Table::atom()) -> #mnesia_players{} | not_found.
read_player_from_table(PlayerNum, Table) ->
    Fun = fun() ->
        case mnesia:read(Table, PlayerNum) of
            [PlayerRecord = #mnesia_players{}] -> PlayerRecord;

            [] -> not_found % should cause an error
        end
    end,
    mnesia:activity(transaction, Fun).


-spec calc_new_coordinates(Record::#mnesia_players{} | #mnesia_bombs{}, Direction::up|down|left|right) -> list().
calc_new_coordinates(Record, Direction) ->
    [X,Y] = case Record of
        #mnesia_players{} ->
            Record#mnesia_players.position;
        #mnesia_bombs{} ->
            Record#mnesia_bombs.position
    end,
    case Direction of
        up -> [X, Y+1];
        down -> [X, Y-1];
        left -> [X-1, Y];
        right -> [X+1,Y]
    end.
%% =====================================================================
%% ? solving the clusterfuck for managing the movement check

%% @doc returns can_move/cant_move/dest_not_here
handle_player_movement(PlayerNum, Direction, State = #gn_state{}) ->
    %% ? calculate the destination coordinate, find out if the coordinate is within limits of current GN (the one initiating the function call)
    Player_record = read_player_from_table(PlayerNum, State#gn_state.players_table_name),
    Destination = calc_new_coordinates(Player_record, Direction),
    Current_gn_name = get_registered_name(self()),
    case get_managing_node_by_coord(hd(Destination),lists:last(Destination)) of
        Current_gn_name -> % destination coordinate is managed by this GN
            %% ? Checks for obstacles in the target coordinate, kickstarting any movements caused by this attempt
            check_for_obstacles(Destination, Player_record#mnesia_players.special_abilities, Direction, State);
        _Other_name ->
            dest_not_here
    end.


check_for_obstacles(Coordinate, BuffsList, Initiator_Direction, State = #gn_state{}) -> 
    %% ? Fetch every entity in that coordinate using QLC
    Entities_at_coord = get_records_at_location(Coordinate, State),
    %% ? Deal with possible interactions - returns can_move/cant_move
    interact_with_entity(Entities_at_coord, BuffsList, Initiator_Direction, State).


%% @doc starts a timer for halfway of the movement (to update the coordinates).
%% returns 'ok' unless record not found (not_found). Does not send any ACK messages, that should be done within the 'main' body
insert_player_movement(PlayerNum, Table) ->
    Fun = fun() ->
        case mnesia:read(Table, PlayerNum, sticky_write) of
            [Player_record = #mnesia_players{}] ->
                Updated_record = Player_record#mnesia_players{
                    movement = {true, erlang:send_after(?TILE_MOVE div Player_record#mnesia_players.speed, self(), {update_coord, player, PlayerNum})}
                    },
                %% Insert updated record into table
                mnesia:write(Updated_record),
                ok;
            [] -> % didn't find 
                not_found
        end
    end,
    mnesia:activity(transaction, Fun).



get_records_at_location(Coordinate, State = #gn_state{}) ->
    Fun = fun() ->
        qlc:eval(qlc:q(
            qlc:append(
                [ {tile, T} || T <- mnesia:table(State#gn_state.tiles_table_name), T#mnesia_tiles.position == Coordinate],
                [ {bomb, B} || B <- mnesia:table(State#gn_state.bombs_table_name), B#mnesia_bombs.position == Coordinate],
                [ {player, P} || P <- mnesia:table(State#gn_state.players_table_name), P#mnesia_players.position == Coordinate]
            ))) end,
        mnesia:activity(transaction, Fun).


-spec interact_with_entity(list(), list(), up|down|left|right, #gn_state{}) -> can_move|cant_move.
interact_with_entity(ListOfEntities, BuffsList, Direction, State) ->
    %% this is the function called upon. rest are the recursion
    interact_with_entity(ListOfEntities, BuffsList, Direction, State, can_move).

-spec interact_with_entity(list(), list(), up|down|left|right, State::#gn_state{}, MoveStatus:: can_move|cant_move) 
    -> can_move|cant_move.
interact_with_entity([], _BuffsList, _Direction, _State, MoveStatus) -> MoveStatus;
interact_with_entity([H|T], BuffsList, Direction, State, MoveStatus) ->
    case H of
        {tile, _Tile} ->
            %% no buffs help with running into a wall, movement request is denied
            interact_with_entity(T, BuffsList, Direction, cant_move);
        {bomb, Bomb} ->
            %% check if can kick bombs, freeze them or phased movement, act accordingly
            Relevant_buffs = [Buff || Buff <- BuffsList, lists:member(Buff, [?KICK_BOMB, ?PHASED, ?FREEZE_BOMB])],
            case Relevant_buffs of
                [] -> 
                    %% no special buffs, can't push bomb, movement is denied
                    interact_with_entity(T, BuffsList, Direction, cant_move);
                [?KICK_BOMB] ->
                    %% kick bomb special buff, tries to initiate a move for the bomb in the movement direction of the player
                    %% ?: send message to self, prompting a bomb's movement request
                    gen_server:cast(self(), 
                        {bomb_kicked, Bomb, Direction}
                    ), % ! prototype for the message format - sends to GN
                    interact_with_entity(T, BuffsList, Direction, cant_move);
                [?PHASED] ->
                    %% can move through bombs. does not cause the bomb to move, able to keep moving
                    interact_with_entity(T, BuffsList, Direction, can_move);
                [?FREEZE_BOMB] ->
                    %% freezes the bomb, cannot move through it
                    %% let the bomb know
                    bomb_as_fsm:freeze_bomb(Bomb#mnesia_bombs.pid),
                    %% update the mnesia table 
                    update_bomb_status(Bomb, State#gn_state.bombs_table_name),
                    interact_with_entity(T, BuffsList, Direction, cant_move)
            end,
            
            ok;
        {player, _Other_player} ->
            %% For now, cannot move through other players - same interaction as with a tile
            interact_with_entity(T, BuffsList, Direction, MoveStatus)
    end.


%% * re-read the bomb and update status to frozen on mnesia table
update_bomb_status(Bomb, Bombs_table) ->
    BombKey = Bomb#mnesia_bombs.position,
    Fun = fun() ->
        [CurrentRecord] = mnesia:wread({Bombs_table, BombKey}),
        mnesia:write(Bombs_table, CurrentRecord#mnesia_bombs{status = frozen}, write)
    end,
    mnesia:activity(transaction, Fun).

%% =====================================================================

-spec get_managing_node_by_coord(X::integer(), Y::integer()) -> atom().
get_managing_node_by_coord(X,Y) when X >= 0, X =< 7, Y > 7, Y =< 15 -> 'GN1_server';
get_managing_node_by_coord(X,Y) when X > 7, X =< 15 , Y > 7 , Y =< 15 -> 'GN2_server';
get_managing_node_by_coord(X,Y) when X >= 0 , X =< 7 , Y >= 0 , Y =< 7 -> 'GN3_server';
get_managing_node_by_coord(X,Y) when X > 7 , X =< 15 , Y >= 0 , Y =< 7 -> 'GN4_server'.


node_name_to_number(Name) ->
    list_to_integer([lists:nth(3, atom_to_list(Name))]).


-spec update_player_direction(PlayerNum::integer(), atom(), atom()) -> term().
update_player_direction(PlayerNum, Table, NewValue) ->
    Fun = fun() ->
        case mnesia:read(Table, PlayerNum, sticky_write) of
            [Player_record = #mnesia_players{}] ->
                Updated_record = Player_record#mnesia_players{direction = NewValue},
                %% Insert updated record into table
                mnesia:write(Updated_record),
                Updated_record;
            [] -> % didn't find 
                not_found
        end
    end,
    mnesia:activity(transaction, Fun).


%% @doc handles the operations needed to be done after given a reply for a movement clearance request to another GN for a player
handle_player_movement_clearance(PlayerNum, Answer, Table_name) ->
    %% ? For debugging purposes ONLY, we are letting the player FSM know of approved move requests. Later on should be only if they are denied
    %% both options (can_move/cant_move) send a reply to the player FSM based on his location - this is done first,
    %% Then the database update occurs (different for both)
    Player_record = read_player_from_table(PlayerNum, Table_name),
    %% respond to the player FSM
    if
        Player_record#mnesia_players.target_gn == Player_record#mnesia_players.local_gn ->
            %% Player FSM is on this node, send message directly
            player_fsm:gn_response(PlayerNum, {move_result, Answer}); 
        true ->
            %% Player FSM is on another machine, forward through CN->local GN
            gen_server:cast(cn_server,
                {forward_request, Player_record#mnesia_players.local_gn,
                    {gn_answer, {move_result, player, PlayerNum, Answer}}
                })
    end,
    case Answer of
        can_move ->
            %% move is possible. Update data, open movement timer
            insert_player_movement(PlayerNum, Table_name);
        cant_move -> % cannot move to the other node
            %% Update direction to none
            case erlang:is_record(update_player_direction(PlayerNum, Table_name, 'none'), mnesia_players) of
                true -> ok;
                _ -> 
                    %% couldn't find the record, crash the process
                    erlang:error(record_not_found, [node(), Player_record])
            end
    end.


handle_bomb_movement_clearance(_BombNum, Answer, _Table_name) -> % todo
    %% ! BombNum and Table_name are "unused" for now to remove warnings until I finish this function
    case Answer of
        can_move->
            placeholder;
        cant_move ->
            placeholder
    end.

-spec check_entered_coord(#mnesia_players{}, State::#gn_state{}) -> ok.
check_entered_coord(Player_record, State) ->
    %% TODO: Check for powerups in new position, if any are found - add their effect to the player's mnesia table entry
    %% TODO: When a powerup is taken it is sent a a 'pickup(Pid)' command to stop & terminate it.
    %% TODO: this powerup entry is removed from the mnesia table
    %% TODO:    (by GN while sending that message / triggered by the termination msg from the powerup process?)
    %% 
    %% TODO: Player FSM should be notified of the following powerup changes: max bombs, speed, (?) lives
    
    Fun = fun() ->
        case mnesia:read(State#gn_state.powerups_table_name, Player_record#mnesia_players.position, write) of
            [] -> ?NO_POWERUP;
            [Found_powerup] -> % a powerup is present at the new position of the player
                %% add powerup to the player (separate function), remove current powerup from table, send msg to process to terminate
                mnesia:delete(State#gn_state.powerups_table_name, Found_powerup, write), % remove powerup from table
                powerup:pickup(Found_powerup#mnesia_powerups.pid), % send msg to terminate process
                Found_powerup#mnesia_powerups.type
        end
        end,
        Powerup = mnesia:activity(transaction, Fun),
        if
            Powerup == ?NO_POWERUP -> ok; % no powerup found in position
            true -> % consume power-up into player, notify player for selected powerups
                consume_powerup(Powerup, Player_record, State#gn_state.players_table_name)
        end.



consume_powerup(Powerup, Player_record, Players_table) ->
    %% TODO: based on current player's powerups, change/update his power in the mnesia table.
    %% TODO: Notify the player FSM 
        
    ok.

%% TODO: general things to do when working:
%% 2. write check_entered_coord
%% 3. Re-write Player FSM to our new needs, update its internal player record