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
-export[handle_player_move_request/2, read_player_from_table/2, calc_new_coordinates/2,
        attempt_player_movement/3, update_player_direction/3, handle_player_movement_clearance/3, handle_bomb_movement_clearance/3].


-export[get_managing_node_by_coord/2, node_name_to_number/1].
-export[get_records_at_location/2].
-export[handle_player_movement/3].

-import(gn_server, [get_registered_name/1]).

-include_lib("project_env/src/Playing_with_Fire_2-Earlang/Code/Objects/common_parameters.hrl").

%%% ===========================================================================
%%% ? BECAUSE WINDOWS IS ANNOYING WITH ERRORS IM INCLUDING THE RECORDS HERE
%-include_lib("project_env/src/Playing_with_Fire_2-Earlang/Code/GameNode/mnesia_records.hrl").
%-include_lib("src/Playing_with_Fire_2-Earlang/Code/Objects/object_records.hrl"). %% ? This should work for compiling under rebar3.

-record(gn_state, {
    tiles_table_name,
    bombs_table_name,
    powerups_table_name,
    players_table_name
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

-record(mnesia_tiles, {
    position, % position - [X,Y]
    type, % can be - unbreakable, breakable, two_hit (-> one_hit)
    contains,  % can be - none (no power-up), bomb (that'll trigger on its own), or any speed-up

    pid = none
}).

-record(mnesia_powerups, {
    position, % position - [X,Y]
    type, % type of power up - can be movement speed, extra bombs etc..
    original_node_ID, % original creating node ID - TODO: unsure of necessity

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
%%% ===========================================================================

handle_player_move_request(_State= #gn_state{}, {PlayerNum, Direction}) -> % TODO
%% ? pre clusterfuck solving
    %% todo: calculate destination coordinate
    %% todo: check if that coordinate is under this GN's control or
    %% todo: if yes: attempt player movement (as detailed in the todo for attempt_player_movement under handle_cast),
    %% todo:         can't move: respond to player FSM 
    %%                     player_fsm:gn_response(Player_record#mnesia_players.pid, {move_result, Answer});
    %% todo:        can move: update mnesia table, open timer for halfway (when we update the player's position)
    %% todo: if not: send message to CN - like below
    gen_server:cast(cn_server, 
        {query_request, get_registered_name(self()), 
            {move_request_out_of_bounds, player, PlayerNum, Destination_coord, Direction}
        }),
    placeholder.


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
    %% ? Deal with possible interactions
    interact_with_entity(Entities_at_coord, BuffsList, Initiator_Direction),
    ok.


get_records_at_location(Coordinate, State = #gn_state{}) ->
    Fun = fun() ->
        qlc:eval(qlc:q(
            qlc:append(
                [ {tile, T} || T <- mnesia:table(State#gn_state.tiles_table_name), T#mnesia_tiles.position == Coordinate],
                [ {bomb, B} || B <- mnesia:table(State#gn_state.bombs_table_name), B#mnesia_bombs.position == Coordinate],
                [ {player, P} || P <- mnesia:table(State#gn_state.players_table_name), P#mnesia_players.position == Coordinate]
            ))) end,
        mnesia:activity(transaction, Fun).


interact_with_entity(ListOfEntities, BuffsList, Direction) ->
    %% this is the function called upon. rest are the recursion
    interact_with_entity(ListOfEntities, BuffsList, Direction, can_move).

interact_with_entity([], BuffsList, Direction, MoveStatus) -> MoveStatus;
interact_with_entity([H|T], BuffsList, Direction, MoveStatus) ->
    case H of
        {tile, Tile} ->
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
                    Direction; %placeholder
                [?PHASED] ->
                    %% can move through bombs. does not cause the bomb to move, able to keep moving
                    interact_with_entity(T, BuffsList, Direction, can_move);
                [?FREEZE_BOMB] ->
                    %% freezes the bomb, cannot move through it
                    placeholder,
                    interact_with_entity(T, BuffsList, Direction, cant_move)
            end,
            
            ok;
        {player, Other_player} ->
            %% For now, cannot move through other players - same interaction as with a tile
            interact_with_entity(T, BuffsList, Direction, MoveStatus)
    end,
    placeholder.




%% =====================================================================

attempt_player_movement(Player=#mnesia_players{}, Direction, State = #gn_state{}) ->
%% ! integrated into handle_player_movement

        Current_gn_name = get_registered_name(self()),
        [X,Y] = Player#mnesia_players.position,
        case get_managing_node_by_coord(X,Y) of
            Current_gn_name -> % destination coordinate is managed by this GN
                placeholder;
                %check_for_obstacles(); % TODO
            _Other_name -> 
                %% ? NOTE FOR LATER: can "skip" the entire query procss in the CN because we already know the
                %% ? name of the GN it should go to, and we can just forward it based on that name.
                %% ? If a GN falls and its data is moved to another node, the name will be re-registered so
                %% ? it should still work.
                dest_not_here
        end.
    



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



handle_player_movement_clearance(PlayerNum, Answer, Table_name) ->
    %% ? pre clusterfuck solving
    case Answer of
        can_move ->
                %% move is possible. Update data, open halfway timer, respond to player FSM
                
                %% todo: update mnesia table - direction and movement
                %% todo: open timer for half-way (when we update the player's position)
                %% respond to the player FSM via CN->hosting GN
                case Player_record#mnesia_players.hosted of % ! Player_record is not defined here, used here for presentation purposes
                    true -> 
                        %% Player FSM is on this machine, send message directly
                        player_fsm:gn_response(Player_record#mnesia_players.pid, {move_result, Answer}); 
                    false ->
                        %% Player FSM is on another machine, forward through CN->local GN
                        gen_server:cast(cn_server,
                            {forward_request, Player_record#mnesia_players.local_gn, % ! HostingGN is placeholder - get it from the record returned by the function that's left todo 
                                {gn_answer, {move_result, player, PlayerNum, accepted}}
                            })
                end;
                
        cant_move -> % cannot move to the other node
            %% Update direction to none, send acknowledge to the player FSM
            Player_record = req_player_move:update_player_direction(PlayerNum, Table_name, 'none'),
            case {erlang:is_record(Player_record),Player_record#mnesia_players.hosted} of
                {true, true} ->
                    %% Player FSM is on this machine, send message directly
                    player_fsm:gn_response(Player_record#mnesia_players.pid, {move_result, Answer}); 
                {true, false} ->
                    %% send message to player FSM through the CN -> hosting GN
                    gen_Server:cast(cn_server,
                        {forward_request, Player_record#mnesia_players.local_gn,
                            {gn_answer, {move_result, player, PlayerNum, denied}}
                        });
                {false,_} ->
                    %% couldn't find the record, crash the process
                    erlang:error(record_not_found, [node(), Player_record])
            end
        end,
    
    ok.

    handle_bomb_movement_clearance(BombNum, Answer, Table_name) ->
        case Answer of
            can_move->
                placeholder;
            cant_move ->
                placeholder
        end.



