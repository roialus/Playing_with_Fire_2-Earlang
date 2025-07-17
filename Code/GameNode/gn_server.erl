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
-include_lib("src/Playing_with_Fire_2-Earlang/Code/Objects/object_records.hrl"). %% ? This should work for compiling under rebar3.

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


-spec(handle_cast(Request :: term(), State :: #gn_state{}) ->
    {noreply, NewState :: #gn_state{}} |
    {noreply, NewState :: #gn_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #gn_state{}}).
%% @doc Handle player requests
handle_cast({player_message, Request}, State = #gn_state{}) ->
    ThisGN = get_registered_name(self),
    case Request of
        {move_request, PlayerNum, ThisGN , Direction} -> % move request from a player in our quarter
            req_player_move:handle_player_move_request(State, {PlayerNum, Direction}), % TODO - separate file
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
            %% ?: handle this using several separated functions for maximal reusability
            %% todo: Check if the destination coordinates is in the current GN's control -
            %% todo: if it isn't: replies with 'dest_not_here'
            %% todo: if it is: check the destination for obstacles (if the move is possible), this should return
            %% todo             'can_move', 'cant_move' while also "kickstart" actions caused by the possible movement like
            %% todo:            kicking/freezing bombs.

            %% extracts the player's record from mnesia's player table
            Player#mnesia_players{} = req_player_move:read_player_from_table(PlayerNum, State#gn_state.players_table_name),

            %% Calculate destination coordinates
            Destination_coord = req_player_move:calc_new_coordinates(Player, Direction),


            %% * as detailed in the massive todo above ^^
            case req_player_move:attempt_player_movement(PlayerNum, Direction, State = #gn_state{}) of
                can_move -> 
                    %% move is possible. Update data, open halfway timer, respond to player FSM

                    %% todo: update mnesia table
                    %% todo: open timer for half-way (when we update the player's position)
                    %% respond to the player FSM via CN->hosting GN
                    gen_server:cast(cn_server,
                        {forward_request, HostingGN, 
                            {gn_answer, {move_result, player, PlayerNum, accepted}}
                        });

                cant_move -> % can't move, obstacle blocking
                    gen_server:cast(cn_server,
                        {forward_request, HostingGN, 
                            {gn_answer, {move_result, player, PlayerNum, denied}}
                        });

                dest_not_here -> % destination coordinate is overseen by another GN
                    gen_server:cast(cn_server,
                        {query_request, get_registered_name(self()), 
                            {move_request_out_of_bounds, player, PlayerNum, Destination_coord, Direction}})
            end,
            {noreply, State};
        
        %% * A GN who hosts a player (physically) receives a response for his movement request
        {move_result, player, PlayerNum, Answer} ->
            %% Pass the message to the Player FSM
            %% Look for the player in your own records (to find his Pid)
            Player_record = req_player_move:read_player_from_table(PlayerNum, State#gn_state.players_table_name),
            case is_record(Player_record) of
                true -> 
                    %% Everything as normal (found the record), pass the message
                    player_fsm:gn_response(Player_record#mnesia_players.pid, {move_result, Answer});
                false -> % crash the process
                    erlang:error(record_not_found, [node(), PlayerRecord])
            end,
            {noreply, State};

        %% * A GN got a respond for a movement request of a player/bomb to another quarter (separate pattern-matching) - this is the forwarded response handler
        %% * This deals with situations where the Player FSM is on the same node as the relevant GN as well as when its on another
        {movement_clearance, player, PlayerNum, Answer} ->
            req_player_move:handle_player_movement_clearance(PlayerNum, Answer, State#gn_state.players_table_name), % todo - complete
            {noreply, State};

        {movement_clearance, bomb, BombIdentifier, Answer} -> % todo
            req_player_move:handle_bomb_movement_clearance(BombIdentifier, Answer, State#gn_state.bombs_table_name),
            {noreply, State};



handle_cast({move_request_out_of_bounds, EntityType, ActualRequest}, State) ->
    %% handling a move request from another GN
    %% differntiate bomb to player
    case EntityType of
        player -> % a player wants to pass to this GN
            {PlayerNum, Destination_coord, Direction, BuffsList, AskingGN} = ActualRequest,
            ReplyingGN = get_registered_name(self()),
            %% todo: check destination coordinate for obstacles (if the move is possible),
            %% todo: this should also "kickstart" any action caused by this attempted movement
            %% todo: returns "can_move" or "cant_move"
            %% * this function should be similar to attempt_player_movement, but slightly different
            Move_result = attempt_player_entrance(Destination_coord, Direction, BuffsList, State),
                gen_server:cast(cn_server, {forward_request, AskingGN,
                    {movement_clearance, player, PlayerNum, Move_result}});
        bomb -> % a bomb wants to pass to this GN
            placeholder %! not yet implemented
    end,
    {noreply, State};

handle_cast(_Request, State = #gn_state{}) -> 
    {noreply, State}.

%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #gn_state{}) ->
    {noreply, NewState :: #gn_state{}} |
    {noreply, NewState :: #gn_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #gn_state{}}).
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
    %% //Player_name = list_to_atom("player_" ++ integer_to_list(GN_number)),
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

