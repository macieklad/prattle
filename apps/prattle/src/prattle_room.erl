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

start_link(Name) ->
    {ok, Pid} = gen_server:start_link({local, Name},
                                      ?MODULE,
                                      [Name],
                                      []),
    log("PID is ~w", Name, [Pid]),
    gen_server:cast(Pid, spawn_client),
    log("Awaiting client connections", Name),
    {ok, Pid}.

init([Name]) ->
    process_flag(trap_exit, true),
    {ok, ListenSocket} = gen_tcp:listen(0,
                                        [binary, {active, true}]),
    {ok, Port} = inet:port(ListenSocket),
    log("Listening on ~w", Name, [Port]),
    {ok,
     #state{socket = ListenSocket, port = Port, clients = [],
            name = Name}}.

handle_call({room_port, RequestedRoom}, _From,
            State = #state{port = Port, name = Name}) ->
    if RequestedRoom == Name -> {reply, Port, State};
       true -> {reply, none, State}
    end;
handle_call(name, _From, State = #state{name = Name}) ->
    {reply, Name, State};
handle_call(_Req, _From, _State) -> {noreply, _State}.

handle_cast({broadcast, Message},
            State = #state{clients = Clients}) ->
    lists:foreach(fun (Client) ->
                          Client ! {broadcast, Message}
                  end,
                  Clients),
    {noreply, State};
handle_cast(spawn_client,
            State = #state{socket = ListenSocket, name = Name}) ->
    spawn_client(Name, self(), ListenSocket),
    {noreply, State};
handle_cast({client_connection, Client},
            State = #state{clients = Clients}) ->
    NewClients = [Client | Clients],
    {noreply, State#state{clients = NewClients}}.

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

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

spawn_client(RoomName, RoomPid, ListenSocket) ->
    prattle_client_conn:spawn(RoomName,
                              RoomPid,
                              ListenSocket).

log(Message, Room) -> log(Message, Room, []).

log(Message, Room, Args) ->
    io:format("[ROOM:~s] " ++ string:trim(Message) ++ "~n",
              [Room] ++ Args).
