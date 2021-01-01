-module(prattle_client_conn).

-import(prattle_room, [log/2, log/3]).

-export([client/3, spawn/3]).

spawn(RoomName, RoomPid, ListenSocket) ->
    spawn_link(?MODULE,
               client,
               [RoomName, RoomPid, ListenSocket]).

client(RoomName, RoomPid, ListenSocket) ->
    Conn = gen_tcp:accept(ListenSocket),
    case Conn of
        {ok, AcceptSocket} ->
            log("Client connection received", RoomName),
            gen_server:cast(RoomPid, {client_connection, self()}),
            send_system_message(AcceptSocket,
                                "Welcome to room: ~w",
                                [RoomName]),
            gen_tcp:controlling_process(AcceptSocket, self()),
            gen_server:cast(RoomPid, spawn_client),
            client_loop(pid_to_list(self()),
                        AcceptSocket,
                        RoomName,
                        RoomPid);
        _Else ->
            log("Client listener didn't accept, closing", RoomName),
            ok
    end.

client_loop(Name, Socket, RoomName, RoomPid) ->
    receive
        {tcp, Socket, <<"msg:", Message/binary>>} ->
            Broadcast = Name ++ " " ++ binary_to_list(Message),
            gen_server:cast(RoomPid, {broadcast, Broadcast});
        {tcp, Socket, <<"name:", RawName/binary>>} ->
            NewName = "<" ++
                          string:trim(binary_to_list(RawName)) ++ ">",
            Message = "Client " ++
                          Name ++ " changed name to " ++ NewName,
            gen_server:cast(RoomPid,
                            {broadcast, system_message(Message)}),
            client_loop(NewName, Socket, RoomName, RoomPid);
        {tcp_closed, ClosedSocket}
            when Socket =:= ClosedSocket ->
            log("User ~s left the room", RoomName, [Name]),
            exit(normal);
        {tcp, _, Message} ->
            log("Received unrecognized command ~s",
                RoomName,
                [binary_to_list(Message)]);
        {broadcast, Message} -> send_message(Socket, Message)
    end,
    client_loop(Name, Socket, RoomName, RoomPid).

send_message(To, String) ->
    send_message(To, String, []).

send_message(To, String, Vars) ->
    gen_tcp:send(To,
                 list_to_binary(io_lib:format(string:trim(String) ++
                                                  "~n",
                                              Vars))).

send_system_message(To, String, Vars) ->
    gen_tcp:send(To,
                 list_to_binary(io_lib:format(system_message(String),
                                              Vars))).

system_message(Message) -> "<<prattle>> " ++ Message.
