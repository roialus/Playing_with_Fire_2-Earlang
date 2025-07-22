%%%-------------------------------------------------------------------
%%% @author dolev
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Jul 2025 11:19
%%%-------------------------------------------------------------------
-module(gn_server).
-author("dolev").

-behaviour(gen_server).

%% API
-export([start_link/1]).

-export[get_registered_name/1].

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-include_lib("mnesia_records.hrl").
%%% Linux compatible
-include_lib("src/clean-repo/Code/Objects/object_records.hrl").
-include_lib("src/clean-repo/Code/common_parameters.hrl").
%%% Windows compatible
%-include_lib("project_env/src/Playing_with_Fire_2-Earlang/Code/Objects/object_records.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers name globally as 'GNx_server', priority set to high 
-spec(start_link({GN_number::integer(), IsBot::boolean()}) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link({GN_number, IsBot}) ->
    GN_name = list_to_atom("GN" ++ integer_to_list(GN_number) ++ "_server"),
    gen_server:start_link({global, GN_name}, ?MODULE, [[GN_number, IsBot]], [{priority, high}]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @doc Initializes the server
-spec(init(list()) ->
    {ok, State :: #gn_state{}} | {ok, State :: #gn_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([GN_number, PlayerType]) ->
    Data = #gn_state{
        tiles_table_name = generate_atom_table_names(GN_number, "_tiles"),
        bombs_table_name = generate_atom_table_names(GN_number, "_bombs"),
        powerups_table_name = generate_atom_table_names(GN_number, "_powerups"),
        players_table_name = generate_atom_table_names(GN_number, "_players")},
    %% Initialize tiles and players from the generated map
    initialize_tiles(Data#gn_state.tiles_table_name),
    initialize_players(Data#gn_state.players_table_name, PlayerType, GN_number),
    {ok, Data}.

%%% ============== Handle call ==============

%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #gn_state{}) ->
    {reply, Reply :: term(), NewState :: #gn_state{}} |
    {reply, Reply :: term(), NewState :: #gn_state{}, timeout() | hibernate} |
    {noreply, NewState :: #gn_state{}} |
    {noreply, NewState :: #gn_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #gn_state{}} |
    {stop, Reason :: term(), NewState :: #gn_state{}}).
handle_call(_Request, _From, State = #gn_state{}) ->
    {reply, ok, State}.

%%% ============== Handle cast ==============

-spec(handle_cast(Request :: term(), State :: #gn_state{}) ->
    {noreply, NewState :: #gn_state{}} |
    {noreply, NewState :: #gn_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #gn_state{}}).
%% @doc Handle player requests
handle_cast({player_message, Request}, State = #gn_state{}) ->
    ThisGN = get_registered_name(self()),
    case Request of
        {move_request, PlayerNum, ThisGN , Direction} -> % move request from a player in our quarter
            Move_verdict = req_player_move:handle_player_movement(PlayerNum, Direction, State),
            case Move_verdict of
                can_move ->
                    req_player_move:insert_player_movement(PlayerNum, State#gn_state.players_table_name),
                    player_fsm:gn_response(PlayerNum, {move_result, Move_verdict}); %% ? this should be removed later on. Stays here for debugging for now
                cant_move ->
                    req_player_move:update_player_direction(PlayerNum, State#gn_state.players_table_name, none),
                    player_fsm:gn_response(PlayerNum, {move_result, Move_verdict});
                dest_not_here ->
                    %% extracts the player's record from mnesia's player table
                    Player = req_player_move:read_player_from_table(PlayerNum, State#gn_state.players_table_name),
                    %% Calculate destination coordinates
                    Destination_coord = req_player_move:calc_new_coordinates(Player, Direction),
                    gen_server:cast(cn_server,
                        {query_request, get_registered_name(self()), 
                            {move_request_out_of_bounds, player, PlayerNum, Destination_coord, Direction}})
            end,
            {noreply, State};
        {move_request, PlayerNum, TargetGN, Direction} -> % move request from a player outside my quarter
            gen_server:cast(cn_server, {forward_request, TargetGN, {move_request, player, PlayerNum, Direction}}),
            {noreply, State}
    end;

handle_cast({forwarded, Request}, State = #gn_state{}) ->
    %% * handles forwarded messages
    case Request of
        {move_request, player, PlayerNum, Direction} ->
            %% *  handles a move request of a player inside my quarter whose FSM is on another node
            %% extracts the player's record from mnesia's player table
            Player = req_player_move:read_player_from_table(PlayerNum, State#gn_state.players_table_name),
            %% Calculate destination coordinates
            Destination_coord = req_player_move:calc_new_coordinates(Player, Direction),

            case req_player_move:handle_player_movement(PlayerNum, Direction, State) of
                can_move -> 
                    %% move is possible. Update data, open halfway timer, respond to player FSM
                    req_player_move:insert_player_movement(PlayerNum, State#gn_state.players_table_name),
                    %% respond to the player FSM via CN->hosting GN
                    gen_server:cast(cn_server,
                        {forward_request, Player#mnesia_players.local_gn, 
                            {gn_answer, {move_result, player, PlayerNum, accepted}}
                        });
                cant_move -> % can't move, obstacle blocking
                    req_player_move:update_player_direction(PlayerNum, State#gn_state.players_table_name, none),
                    gen_server:cast(cn_server,
                        {forward_request, Player#mnesia_players.local_gn, 
                            {gn_answer, {move_result, player, PlayerNum, denied}}
                        });
                dest_not_here -> % destination coordinate is overseen by another GN
                    gen_server:cast(cn_server,
                        {query_request, get_registered_name(self()), 
                            {move_request_out_of_bounds, player, PlayerNum, Destination_coord, Direction}})
            end,
            {noreply, State};
        
        %% * A GN who hosts a player (physically) receives a response for his movement request
        {gn_answer, {move_result, player, PlayerNum, Answer}} ->
            %% Pass the message to the Player FSM
            %% Look for the player in your own records (to find his Pid)
            Player_record = req_player_move:read_player_from_table(PlayerNum, State#gn_state.players_table_name),
            case erlang:is_record(Player_record, mnesia_players) of 
                true -> 
                    %% Everything as normal (found the record), pass the message
                    player_fsm:gn_response(PlayerNum, {move_result, Answer});
                false -> % crash the process
                    erlang:error(record_not_found, [node(), Player_record])
            end,
            {noreply, State};

        %% * A GN got a respond for a movement request of a player/bomb to another quarter (separate pattern-matching) - this is the forwarded reply handler
        %% * This deals with situations where the Player FSM is on the same node as the relevant GN as well as when its on another
        {movement_clearance, player, PlayerNum, Answer} ->
            req_player_move:handle_player_movement_clearance(PlayerNum, Answer, State#gn_state.players_table_name),
            {noreply, State};

        {movement_clearance, bomb, BombIdentifier, Answer} -> % todo
            req_player_move:handle_bomb_movement_clearance(BombIdentifier, Answer, State#gn_state.bombs_table_name),
            {noreply, State};

        %% ! ***REMOVE***
        %% ** Forwarded messages regarding the actual movement update for a player
        {update_coords, player, PlayerNum, New_coord} ->
            %% pass the message to the player FSM
            Player_record = req_player_move:read_player_from_table(PlayerNum, State#gn_state.players_table_name),
            case erlang:is_record(Player_record, mnesia_players) of 
                true -> 
                    %% Everything as normal (found the record), pass the message
                    player_fsm:gn_response(PlayerNum, {update_coords, New_coord});
                false -> % crash the process
                    erlang:error(record_not_found, [node(), Player_record])
            end,
            {noreply, State};

        %% * A player has changed coordinates, resulting in a target GN change.
        %% * this message is sent by the previous GN to let the player FSM update his target
        {new_target_gn, player, PlayerNum, New_GN} ->
            player_fsm:gn_response(PlayerNum, {new_target_gn, New_GN})
        
        
    end;


%% * received a move request to the quarter of this GN from another GN - differntiate bomb from player
handle_cast({move_request_out_of_bounds, EntityType, ActualRequest}, State) ->
    case EntityType of
        player -> % a player wants to pass to this GN
            {PlayerNum, Destination_coord, Direction, BuffsList, AskingGN} = ActualRequest,
            %% checks destination coordinate for obstacles (if the move is possible),
            %% this should also "kickstart" any action caused by this attempted movement
            Move_result = req_player_move:check_for_obstables(Destination_coord, BuffsList, Direction, State),
            gen_server:cast(cn_server, 
                {forward_request, AskingGN,
                    {movement_clearance, player, PlayerNum, Move_result}});
        bomb -> % a bomb wants to pass to this GN
            placeholder %! not yet implemented
    end,
    {noreply, State};


%% * A player came into my quarter of the map - open a timer, let the player FSM know
handle_cast({incoming_player, PlayerNum}, State) ->
    Player_record = req_player_move:read_player_from_table(PlayerNum, State#gn_state.players_table_name),
    req_player_move:check_entered_coord(Player_record), % TODO - not written yet!
    {noreply, State};


%% * this is a catch-all&ignore clause
handle_cast(_Request, State = #gn_state{}) -> 
    {noreply, State}.

%%% ============== Handle info ==============

%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #gn_state{}) ->
    {noreply, NewState :: #gn_state{}} |
    {noreply, NewState :: #gn_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #gn_state{}}).


%% * Timer for finishing a movement by the player has expired.
%% * Checks if the player stays in current GNs boundaries
%% * If it changes GNs, transfer the players table entry to the new GN, and update the Player FSM of that change
%% * Regardless, checks destination for power-ups/explosions
%% ! explosions are a tricky thing - for now im not going to do it; the explosion checks everything in its radius when it explodes and that's it
handle_info({update_coord, player, PlayerNum}, State = #gn_state{}) ->
        case req_player_move:read_and_update_coord(player, PlayerNum, State#gn_state.players_table_name) of
            not_found -> % got an error somewhere, crash the process. this is mostly for debugging as of now
                erlang:error(failure_when_updating_record, [node(), PlayerNum]);
            {same_gn, Player_record} ->
                if 
                    Player_record#mnesia_players.target_gn == Player_record#mnesia_players.local_gn ->
                        %% player FSM is on the same machine as the GN
                        req_player_move:check_entered_coord(Player_record, State); % TODO - not written yet!
                    true -> %% player FSM is on another machine, forward message through CN->local GN
                        ok 
                end;
            {switch_gn, Player_record, Current_GN, New_GN} ->
                %% transfer records to new GN
                gen_server:cast(cn_server,
                    {transfer_records, player, PlayerNum, Current_GN, New_GN}),
                %% Let player FSM know of the GN change
                Player_local_gn = Player_record#mnesia_players.local_gn,
                %% Check where Player FSM is physically - current node or someplace else
                case get_registered_name(self()) of
                    Player_local_gn -> %% player FSM on this node
                        player_fsm:gn_response(PlayerNum, {new_target_gn, New_GN});
                    _ -> %% Player FSM on another node
                        gen_server:cast(cn_server,
                            {forward_request, Player_local_gn, 
                                {new_target_gn, player, PlayerNum, New_GN}})
                        
                end
        end,
        {noreply, State};


%% * default, catch-all and ignore
handle_info(_Info, State = #gn_state{}) ->
{noreply, State}.

%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #gn_state{}) -> term()).
terminate(_Reason, _State = #gn_state{}) ->
ok.

%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #gn_state{},
Extra :: term()) ->
{ok, NewState :: #gn_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #gn_state{}, _Extra) ->
{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc returns the registered name of a Pid
get_registered_name(Pid) ->
    {_, Registered_name} = process_info(Pid, registered_name),
    Registered_name.

%% @doc helper function to create mnesia table names
generate_atom_table_names(Number, Type) ->
    list_to_atom("gn" ++ integer_to_list(Number) ++ Type).

initialize_tiles(TableName) ->
    Fun = fun() ->
        AllRecords = mnesia:match_object(TableName, #mnesia_tiles{_ = '_'}, read),
        lists:foreach(
            fun(Tile) ->
            % spawn a tile gen_server process for each tile in the table
                [Pos_x, Pos_y] = Tile#mnesia_tiles.position,
                [Type, Contains] = [Tile#mnesia_tiles.type, Tile#mnesia_tiles.contains],
                {ok, Pid} = tile:start_link(Pos_x, Pos_y, Type, Contains),
                UpdatedRecord = Tile#mnesia_tiles{pid = Pid},
                mnesia:write(TableName, UpdatedRecord, write)
                end,
            AllRecords), ok
        end,
    mnesia:activity(transaction, Fun).

-spec initialize_players(TableName:: atom(), PlayerType:: atom(), GN_number::1|2|3|4) -> term().
initialize_players(TableName, PlayerType, GN_number) ->
    %% start io_handler gen_server
    {ok, IO_pid} = io_handler:start_link(GN_number, PlayerType),
    Fun = fun() ->
        [PlayerRecord = #mnesia_players{}] = mnesia:read(TableName, GN_number),
        {ok, FSM_pid} = player_fsm:start_link(GN_number, PlayerRecord#mnesia_players.position, self(),
         PlayerType, IO_pid),
        %% update FSM Pid in the IO process
        ok = io_handler:set_player_pid(IO_pid, FSM_pid),
        %% Update mnesia record
        UpdatedRecord = PlayerRecord#mnesia_players{
            local_gn = node(),
            local_gn_pid = self(),
            target_gn = node(), % by default starts at his own GN's quarter
            io_handler_pid = IO_pid,
            pid = FSM_pid,
            bot = PlayerType},
        mnesia:write(TableName, UpdatedRecord, write)
        end,
    mnesia:activity(transaction, Fun).

