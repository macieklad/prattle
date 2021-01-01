-module(client).

-export([start/0, start/1, stop/1]).

-export([prompt/1, strip_tokens/2]).

start() ->
    log("Welcome to prattle chat!"),
    log("Entering lobby, use join:room_name to "
        "join or create chat room."),
    log("If you want to know existing room, use "
        "room:list command"),
    log("If you want to exit the app, use prattle:leav"
        "e command"),
    Client = spawn_link(client,
                        start,
                        [{{127, 0, 0, 1}, 8000}]),
    prompt(Client).

start({ServerHost, ServerPort}) ->
    Conn = gen_tcp:connect(ServerHost,
                           ServerPort,
                           [binary, {active, true}]),
    case Conn of
        {ok, ServerSocket} ->
            lobby(ServerSocket),
            gen_tcp:close(ServerSocket);
        {error, Reason} ->
            log("Could not connect to main chat server: ~s",
                [Reason]),
            io:format("Retrying in 5 seconds. ~n"),
            timer:sleep(5000)
    end,
    start({ServerHost, ServerPort}).

stop(_State) -> ok.

prompt(Client) ->
    timer:sleep(100),
    Prompt = io:get_line(">>"),
    Client ! {prompt, Prompt},
    prompt(Client).

lobby(ServerSocket) ->
    receive
        {prompt, Content} ->
            case strip_tokens(Content, ":") of
                ["join", Room] ->
                    send(ServerSocket, "join:" ++ Room),
                    receive
                        {tcp,
                         ServerSocket,
                         <<"room_port:", RoomPort/binary>>} ->
                            {ok, ChatSocket} = gen_tcp:connect({127, 0, 0, 1},
                                                               list_to_integer(binary_to_list(RoomPort)),
                                                               [binary,
                                                                {active,
                                                                 true}]),
                            chat(ChatSocket)
                    end;
                ["room", "list"] ->
                    send(ServerSocket, "room:list"),
                    receive
                        {tcp, ServerSocket, <<"rooms:", Rooms/binary>>} ->
                            log(binary_to_list(Rooms)),
                            lobby(ServerSocket);
                        Message -> erlang:display(Message)
                    end;
                ["prattle", "leave"] ->
                    gen_tcp:close(ServerSocket),
                    exit(normal);
                _Else ->
                    log("Invalid command ~s provided", [Content]),
                    lobby(ServerSocket)
            end;
        {tcp, _From, Response} ->
            log("Received unrecognized response: ~w", [Response]),
            lobby(ServerSocket);
        Message ->
            log("Received unrecognized client message ~w",
                [Message]),
            lobby(ServerSocket)
    end.

chat(Socket) ->
    receive
        {prompt, Content} ->
            case strip_tokens(Content, ":") of
                ["leave", "room"] ->
                    log("Room left, going back to lobby"),
                    gen_tcp:close(Socket);
                ["name", Name] ->
                    send(Socket, "name:" ++ Name),
                    chat(Socket);
                _Else ->
                    send(Socket, "msg:" ++ Content),
                    chat(Socket)
            end;
        {tcp, _, <<Response/binary>>} ->
            io:format(binary_to_list(Response)),
            chat(Socket);
        {tcp_closed, ClosedSocket} ->
            if Socket =:= ClosedSocket ->
                   log("Connection to room closed, leaving channel");
               true -> chat(Socket)
            end;
        Message ->
            log("Chat received unrecognized client message ~w",
                [Message]),
            chat(Socket)
    end.

log(Message) -> log(Message, []).

log(Message, Args) ->
    io:format("<<prattle>> " ++ Message ++ "~n", Args).

send(Socket, Message) ->
    gen_tcp:send(Socket,
                 list_to_binary(string:trim(Message))).

strip_tokens(S, Sep) ->
    [string:trim(X) || X <- string:tokens(S, Sep)].