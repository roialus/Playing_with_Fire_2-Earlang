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

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).


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
    case Request of
        {move_request, PlayerNum, node() , Direction} -> % move request from a player in our quarter
            handle_player_move_request(State, {PlayerNum, Direction}), % TODO
            {noreply, State};
        {move_request, PlayerNum, TargetGN, Direction} -> % move request from a player outside my quarter
            gen_server:cast(cn_server, {forward_request, {player_message, Request}}),
            {noreply, State}
    end;

%% @doc Handle forwarded requests
handle_cast({forwarded, Request}, State = #gn_state{}) ->
    case Request of
        {player_message, {move_request, PlayerNum, Direction}} ->
            %% *: handle this using several separated functions for maximal reusability
            %% todo: Check if the destination coordinates is in the current GN's control -
            %% todo: if it isn't: replies with 'dest_not_here'
            %% todo: if it is: check the destination for obstacles (if the move is possible), this should return
            %% todo             'can_move', 'cant_move' while also "kickstart" actions caused by the possible movement like
            %% todo:            kicking/freezing bombs.
            
            %% TODO: extracts the player's record from mnesia's player table
            Player#mnesia_players{} = placeholder,

            %% TODO: calculate destination coordinates
            Destination_coord = placeholder,

            %% * as detailed in the massive todo above ^^
            case attempt_player_movement(PlayerNum, Direction, State = #gn_state{}) of
                can_move -> 
                    %% todo: update mnesia table
                    %% todo: open timer for half-way (when we update the player's position)
                    %% send an ACK to the player FSM via CN->hosting GN
                    gen_server:cast(cn_server,
                        {forward_request, {gn_answer, HostingGN, {move_request, accepted, PlayerNum}}});
                        %% todo: update mnesia table, open a timer for 'half-way' of the movement
                cant_move -> % can't move, obstacle blocking
                    gen_server:cast(cn_server,
                        {forward_request, {gn_answer, HostingGN, {move_request, denied, PlayerNum}}});
                dest_not_here -> % destination coordinate is overseen by another GN
                    {_, GN_registered_name} = process_info(self(), registered_name),
                    gen_server:cast(cn_server,
                        {req_exceeds_gn, {player_move_request, PlayerNum, Destination_coord, Direction, GN_registered_name}})
            end,
            {noreply, State};

        {move_request, Answer, PlayerNum} -> % GN answered the movement request
            %% todo: query the mnesia player table by the PlayerNum to get his Pid
            Player_record = read_player_from_table(PlayerNum, State#gn_state.players_table_name),
            case is_record(Player_record) of
                true -> 
                    %% Pass message to player FSM
                    player_fsm:gn_response(Player_record#mnesia_players.pid, {move_result, Answer}),
                    {noreply, State}
                false -> % crash the process
                    erlang:error(record_not_found, [node(), PlayerRecord])
            end

        {checking_movement_possibility, {PlayerNum, Destination_coord, Direction, BuffsList, AskingGN}} ->
            %% todo: check destination coordinate for obstacles (if the move is possible),
            %% todo: this should also "kickstart" any action caused by this attempted movement
            %% todo: returns "can_move" or "cant_move"
            %% * this function should be similar to attempt_player_movement, but slightly different
            {_, ReplyingGN} = process_info(self(), registered_name),
            case attempt_player_entrance(Destination_coord, Direction, BuffsList, State) of
                %% ! both these messages are unaddressed in the cn_server right now - need to embrace a convention for thie message's layout
                can_move -> 
                    gen_server:cast(cn_server, {forward_request, AskingGn, {movement_clearance, can_move, PlayerNum, ReplyingGN}});
                cant_move ->
                    gen_server:cast(cn_server, {forward_request, AskingGn, {movement_clearance, cant_move, PlayerNum, ReplyingGN}});
            end
            {noreply, State};

            

    end;



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


-spec read_player_from_table(PlayerNum::integer(), record()) -> record().
read_player_from_table(PlayerNum, Table) ->
    Fun = fun() ->
        case mnesia:read(Table, PlayerNum) of
            [PlayerRecord = #mnesia_players{}] -> PlayerRecord;
        [] -> not_found; % should cause an error
        end,
    end,
    mnesia:activity(transaction, Fun).