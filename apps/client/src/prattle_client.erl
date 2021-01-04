-module(prattle_client).

-export([start/0, start/1, stop/0]).

-export([prompt/1, strip_tokens/2]).

%% First we will start the client with user provided or default args.
%% Client will start configuration store and prompt proccesses,
%% and then will try to connect to server and enter lobby.
start() -> start({{127, 0, 0, 1}, 8000}).

start({ServerHost, ServerPort}) ->
    prattle_store:start([{host, ServerHost},
			 {port, ServerPort}]),
    log("Welcome to prattle chat!"),
    log("Entering lobby, use join:room_name to "
	"join or create chat room."),
    log("If you want to know existing room, use "
	"room:list command"),
    log("If you want to exit the app, use prattle:leav"
	"e command"),
    register(prompt, spawn_link(?MODULE, prompt, [self()])),
    lobby(connect()).

%% When we terminate the client, we make sure to kill
%% dangling store and prompt proccessses.
stop() ->
    log("Stopping prattle client, bye!"),
    exit(whereis(prompt), kill),
    exit(whereis(store_instance), kill),
    exit(normal).

%% Simple prompt process, it will accept user input
%% and pass it to main client loop, we use sleep
%% to rate limit the prompt, just in case.
prompt(Client) ->
    timer:sleep(100),
    Prompt = io:get_line(">>"),
    Client ! {prompt, Prompt},
    prompt(Client).

%% See architecture section in readme to know what happens
%% after we connect to lobby, internal comments will
%% explain special fragments of code.
lobby(ServerSocket) ->
    receive
      %% If user will send input, react to it
      {prompt, Content} ->
	  %% We use colons across the app to split command name and arguments
	  %% strip_tokens helper will ensure that no whitespace is included
	  %% in command parts after the splitting is done.
	  case strip_tokens(Content, ":") of
	    %% User requested room join, ask server for room port,
	    %% and estabilish connection with it. If everything
	    %% is fine, start the chatroom loop.
	    ["join", Room] ->
		send(ServerSocket, "join:" ++ Room),
		receive
		  {tcp, ServerSocket,
		   <<"room_port:", RoomPort/binary>>} ->
		      ServerHost = prattle_store:take(host),
		      {ok, ChatSocket} = gen_tcp:connect(ServerHost,
							 list_to_integer(binary_to_list(RoomPort)),
							 [binary,
							  {active, true}]),
		      log("Entering chat, type anything and press "
			  "enter to send message, type name:YOUR_NAME "
			  "to set nickname or leave:room to go "
			  "back to lobby"),
		      chat(ChatSocket)
		end;
	    %% User requested room list, ask server for the response,
	    %% and stay in lobby, as room join was not requested.
	    ["room", "list"] ->
		send(ServerSocket, "room:list"),
		receive
		  {tcp, ServerSocket, <<"rooms:", Rooms/binary>>} ->
		      log(binary_to_list(Rooms)), lobby(ServerSocket);
		  Message -> erlang:display(Message)
		end;
	    %% User requested app leave, so we terminate the app
	    %% and close the port to clean up.
	    ["prattle", "leave"] ->
		gen_tcp:close(ServerSocket), stop();
	    %% Catch all handler for bad commands
	    _Else ->
		log("Invalid command ~s provided", [Content]),
		lobby(ServerSocket)
	  end;
      %% Server shouldn't send bad responses
      {tcp, _From, Response} ->
	  log("Received unrecognized response: ~w", [Response]),
	  lobby(ServerSocket);
      %% Server may terminate for some reason, so we reconnect
      {tcp_closed, _} ->
	  log("Server closed connection, reentering "
	      "lobby"),
	  lobby(connect());
      %% Other messages shouldn't come here.
      Message ->
	  log("Received unrecognized client message ~w",
	      [Message]),
	  lobby(ServerSocket)
    end.

%% Commands are handled the same way as in the lobby room,
%% sockets are in binary mode, so we match against
%% binary string and cast them to lists whenever
%% needed to use the messages.
chat(Socket) ->
    receive
      {prompt, Content} ->
	  case strip_tokens(Content, ":") of
	    ["leave", "room"] ->
		log("Room left, going back to lobby"),
		gen_tcp:close(Socket);
	    ["name", Name] ->
		send(Socket, "name:" ++ Name), chat(Socket);
	    _Else -> send(Socket, "msg:" ++ Content), chat(Socket)
	  end;
      {tcp, _, <<Response/binary>>} ->
	  %% Responses shouldn't go through log function,
	  %% as they are raw strings to be displayed
	  io:format(binary_to_list(Response)),
	  chat(Socket);
      %% If the room will close by some reason, we fall back to lobby
      {tcp_closed, ClosedSocket} ->
	  if Socket =:= ClosedSocket ->
		 log("Connection to room closed, going back "
		     "to lobby");
	     true -> chat(Socket)
	  end;
      Message ->
	  log("Chat received unrecognized client message ~w",
	      [Message]),
	  chat(Socket)
    end,
    lobby(connect()).

%% Tcp connections may fail, in case of rooms we (with hope) assume that they exist
%% and work well, but while first connecting to server we would rather try
%% that the connection is estabilished, and that is done here.
%% Client will retry connections every 5 seconds.
%%
%% Whole point of external configuration store is seen here, as the connect function
%% is used in a lot of places, so passing configuration would be a hassle,
%% so we use external proccess for that.
connect() ->
    ServerHost = prattle_store:take(host),
    ServerPort = prattle_store:take(port),
    Conn = gen_tcp:connect(ServerHost, ServerPort,
			   [binary, {active, true}]),
    case Conn of
      {ok, ServerSocket} ->
	  log("Connected to lobby."), ServerSocket;
      {error, Reason} ->
	  log("Could not connect to main chat server: ~s",
	      [Reason]),
	  io:format("Retrying in 5 seconds. ~n"),
	  timer:sleep(5000),
	  connect()
    end.

%% Helper functions to abstract repeatable tasks.
log(Message) -> log(Message, []).

log(Message, Args) ->
    io:format("<<prattle>> " ++ Message ++ "~n", Args).

send(Socket, Message) ->
    gen_tcp:send(Socket,
		 list_to_binary(string:trim(Message))).

strip_tokens(S, Sep) ->
    [string:trim(X) || X <- string:tokens(S, Sep)].
