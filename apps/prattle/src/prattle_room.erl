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

-export([client/2]).

-record(state, {socket, port, clients}).

start_link(Name) ->
    {ok, Pid} = gen_server:start_link({local, Name},
                                      ?MODULE,
                                      [],
                                      []),
    gen_server:cast(Pid, spawn_client),
    io:format("Awaiting client connections: ~n").

init(_Args) ->
    process_flag(trap_exit, true),
    {ok, ListenSocket} = gen_tcp:listen(0,
                                        [binary, {active, true}]),
    {ok, Port} = inet:port(ListenSocket),
    io:format("Listening on ~w ~n", [Port]),
    {ok,
     #state{socket = ListenSocket, port = Port,
            clients = []}}.

handle_call(_Req, _From, _State) -> {noreply, _State}.

handle_cast({broadcast, Message},
            State = #state{clients = Clients}) ->
    lists:foreach(fun (Client) ->
                          Client ! {broadcast, Message}
                  end,
                  Clients),
    {noreply, State};
handle_cast(spawn_client,
            State = #state{socket = ListenSocket,
                           clients = Clients}) ->
    Client = spawn_client(self(), ListenSocket),
    NewClients = [Client | Clients],
    {noreply, State#state{clients = NewClients}}.

handle_info({'EXIT', From, _},
            State = #state{clients = Clients}) ->
    erlang:display(Clients),
    NewClients = lists:filter(fun (Client) ->
                                      Client =/= From
                              end,
                              Clients),
    NewState = State#state{clients = NewClients},
    {noreply, NewState}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

spawn_client(RoomPid, ListenSocket) ->
    spawn_link(?MODULE, client, [RoomPid, ListenSocket]).

client(Room, ListenSocket) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    io:format("Client connection received ~n"),
    gen_tcp:send(AcceptSocket,
                 io_lib:format("Welcome to room: ~w ~n", [Room])),
    gen_tcp:controlling_process(AcceptSocket, self()),
    gen_server:cast(Room, spawn_client),
    client_loop(self(), AcceptSocket, Room).

client_loop(Name, Socket, Room) ->
    receive
        {tcp, Socket, <<"msg:", Message/binary>>} ->
            Pid = pid_to_list(self()),
            Broadcast = lists:merge([Pid, binary_to_list(Message)]),
            gen_server:cast(Room,
                            {broadcast, list_to_binary(Broadcast)});
        {tcp, _, Message} ->
            io:format("Received unrecognized command ~s ~n",
                      [binary_to_list(Message)]);
        {broadcast, Message} -> gen_tcp:send(Socket, Message)
    end,
    client_loop(Name, Socket, Room).
