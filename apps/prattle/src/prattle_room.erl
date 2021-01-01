%%%-------------------------------------------------------------------
%% @doc Single chat room server
%% @end
%%%-------------------------------------------------------------------

-module(prattle_room).

-behaviour(gen_server).

%% API
-export([start_link/1]).

-export([code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         terminate/2]).

-export([log/2, log/3]).

-record(state, {socket, port, clients, name}).

%% Start new room, this will be called by supervisor,
%% proccess details are logged to console.
start_link(Name) ->
    {ok, Pid} = gen_server:start_link({local, Name},
                                      ?MODULE,
                                      [Name],
                                      []),
    log("PID is ~w", Name, [Pid]),
    %% Rooms are not handling client connections, dedicated proccesses
    %% do. We ask the spawned room to create new client connection.
    %% After the user connects, requests can and will be sent
    %% again to the spawn_client function, which will
    %% prepare another handlers.
    gen_server:cast(Pid, spawn_client),
    log("Awaiting client connections", Name),
    {ok, Pid}.

%% Room will listen on first free port provided by the system,
%% and then will register basic information inside its state.
init([Name]) ->
    process_flag(trap_exit, true),
    {ok, ListenSocket} = gen_tcp:listen(0,
                                        [binary, {active, true}]),
    {ok, Port} = inet:port(ListenSocket),
    log("Listening on ~w", Name, [Port]),
    {ok,
     #state{socket = ListenSocket, port = Port, clients = [],
            name = Name}}.

%% Room port requests will be sent to all rooms, but only
%% the one with the exact name will respond with numeric
%% port to the server, other will return "none" atom.
handle_call({room_port, RequestedRoom}, _From,
            State = #state{port = Port, name = Name}) ->
    if RequestedRoom == Name -> {reply, Port, State};
       true -> {reply, none, State}
    end;
%% Room list command will ask each room proccess for its name.
handle_call(name, _From, State = #state{name = Name}) ->
    {reply, Name, State};
%% Other calls are ignored and will time out.
handle_call(_Req, _From, _State) -> {noreply, _State}.

%% Client connection will receive messages from client,
%% then it will send broadcast request to parent room,
%% and it will re-transmit it to the children.
%% They shall send it back to the users.
handle_cast({broadcast, Message},
            State = #state{clients = Clients}) ->
    lists:foreach(fun (Client) ->
                          Client ! {broadcast, Message}
                  end,
                  Clients),
    {noreply, State};
%% Client handler requests are handled here. We spawn
%% new handler for each request, and await requests.
handle_cast(spawn_client,
            State = #state{socket = ListenSocket, name = Name}) ->
    spawn_client(Name, self(), ListenSocket),
    {noreply, State};
%% Client connected to the handler, we register the handler
%% as valid client, if broadcast will come, clients on
%% the list will be notified about it.
handle_cast({client_connection, Client},
            State = #state{clients = Clients}) ->
    NewClients = [Client | Clients],
    {noreply, State#state{clients = NewClients}}.

%% Rooms are trapping exit codes from child processes,
%% when client leaves the room it will result in
%% exit code passed here, then we deregister the
%% client, and if no are left, we can safely
%% close the whole chat room.
handle_info({'EXIT', From, _},
            State = #state{clients = Clients, name = Name}) ->
    NewClients = lists:filter(fun (Client) ->
                                      Client =/= From
                              end,
                              Clients),
    NewState = State#state{clients = NewClients},
    if NewClients =:= [] ->
           log("Last client left the room, shutting "
               "it down",
               Name),
           exit(normal);
       true -> ok
    end,
    {noreply, NewState}.

%% Empty calls from gen_server behaviour
terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Helper functions
spawn_client(RoomName, RoomPid, ListenSocket) ->
    prattle_client_conn:spawn(RoomName,
                              RoomPid,
                              ListenSocket).

log(Message, Room) -> log(Message, Room, []).

log(Message, Room, Args) ->
    io:format("[ROOM:~s] " ++ string:trim(Message) ++ "~n",
              [Room] ++ Args).
