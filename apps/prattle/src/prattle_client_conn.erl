%%%-------------------------------------------------------------------
%% @doc Client connection proccess
%% @end
%%%-------------------------------------------------------------------
-module(prattle_client_conn).

-import(prattle_room, [log/2, log/3]).

-export([client/3, spawn/3]).

%% Each client is handled separately, making the app asynchronous,
%% We don't need to wait for socket accept calls to proceed
%% with other connections and messages. Connection
%% proccesses are spawned from room module.
%% See it first to know what happens here.
spawn(RoomName, RoomPid, ListenSocket) ->
    spawn_link(?MODULE,
               client,
               [RoomName, RoomPid, ListenSocket]).

%% Connection initialization, we wait for a client to connect,
%% take control over the socket to enable writing to it,
%% and then we enter the loop and start chatting.
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
            %% When client connection is established, we send request
            %% to the room to tell it that new connection should be
            %% prepared, as another client may come. We are
            %% preparing only one connection in advance,
            %% but it could be done in batches to
            %% prevent cold starts.
            gen_server:cast(RoomPid, spawn_client),
            client_loop(pid_to_list(self()),
                        AcceptSocket,
                        RoomName,
                        RoomPid);
        %% Something may happen along the way, for example room may exit,
        %% as no clients will be left. This must lead to termination
        %% of child connection proccesses, and we catch it here,
        %% otherwise supervisor will throw error in the shell.
        _Else ->
            log("Client listener didn't accept, closing", RoomName),
            ok
    end.

%% Client loop, accepts incoming strings, parses them
%% and executes actions bases on the content.
client_loop(Name, Socket, RoomName, RoomPid) ->
    receive
        %% All messages should first go to the room,
        %% and it will broadcast them. We wiil
        %% send response as the result of
        %% incoming broadcast message.
        {tcp, Socket, <<"msg:", Message/binary>>} ->
            Broadcast = Name ++ " " ++ binary_to_list(Message),
            gen_server:cast(RoomPid, {broadcast, Broadcast});
        %% If user sends name change command, we update it
        %% by calling the loop with different params.
        {tcp, Socket, <<"name:", RawName/binary>>} ->
            NewName = "<" ++
                          string:trim(binary_to_list(RawName)) ++ ">",
            Message = "Client " ++
                          Name ++ " changed name to " ++ NewName,
            gen_server:cast(RoomPid,
                            {broadcast, prattle_utils:system_message(Message)}),
            client_loop(NewName, Socket, RoomName, RoomPid);
        %% If connection socket is closed, it means that user left the room
        {tcp_closed, ClosedSocket}
            when Socket =:= ClosedSocket ->
            log("User ~s left the room", RoomName, [Name]),
            exit(normal);
        {tcp, _, Message} ->
            log("Received unrecognized command ~s",
                RoomName,
                [binary_to_list(Message)]);
        %% Message will be sent only when room requests that
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
                 list_to_binary(io_lib:format(prattle_utils:system_message(String)
                                                  ++ "~n",
                                              Vars))).
