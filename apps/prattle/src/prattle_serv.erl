%%%-------------------------------------------------------------------
%% @doc Main prattle server
%% @end
%%%-------------------------------------------------------------------

-module(prattle_serv).

-behaviour(gen_server).

%% API
-export([start_link/1]).

-export([code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         terminate/2]).

-export([handle_connection/2, listen/2]).

-record(state, {socket}).

%% First we will start the server, and cast the listen
%% call on it to start listening for conections on
%% a constant port.
start_link({ServerPort}) ->
    log("Starting prattle on port ~w", [ServerPort]),
    {ok, ServerSocket} = gen_tcp:listen(ServerPort,
                                        [{active, true}, binary]),
    {ok, Serv} = gen_server:start_link({local, ?MODULE},
                                       ?MODULE,
                                       [ServerSocket],
                                       []),
    gen_server:cast(Serv, listen),
    {ok, Serv}.

init([ListenSocket]) ->
    {ok, #state{socket = ListenSocket}}.

%% Clients will ask for room ports, if none is found new
%% room is created and its port is returned.
handle_call({room_port, Room}, _From, State) ->
    Port = room_port(Room),
    if Port == none -> {reply, create_room(Room), State};
       true -> {reply, Port, State}
    end.

%% Listen request will come only once at the beggining of the
%% app, we spawn connection handler for each lobby user.
handle_cast(listen,
            State = #state{socket = ListenSocket}) ->
    listen(self(), ListenSocket),
    log("Listening, awaiting on connections"),
    {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

%% If server shall terminate (because its supervisor will
%% request that if app closes), we gracefully close
%% the socket and free the underlying port.
terminate(_Reason,
          State = #state{socket = ListenSocket}) ->
    gen_tcp:close(ListenSocket),
    {ok, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

listen(Server, ListenSocket) ->
    spawn_link(?MODULE,
               handle_connection,
               [Server, ListenSocket]).

%% Main connection handler, if someone connects, new
%% handler will be spun up for future connections,
%% and the current one enters the loop
%% to handle incoming commands
handle_connection(Server, ListenSocket) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    listen(Server, ListenSocket),
    handle_messages(Server, AcceptSocket),
    gen_tcp:close(AcceptSocket).

%% Message handling loop, we are accepting binary
%% strings from the sockets, parse them,
%% and depnding on the command, request
%% correct behaviour from the server.
%%
%% After the request is executed, response
%% shall be sent to the client.
handle_messages(Server, AcceptSocket) ->
    receive
        {tcp, _, <<"join:", Room/binary>>} ->
            RoomPort = gen_server:call(Server,
                                       {room_port,
                                        string:trim(binary_to_list(Room))}),
            Message = "room_port:" ++ integer_to_list(RoomPort),
            gen_tcp:send(AcceptSocket, list_to_binary(Message));
        {tcp, _, <<"room:list">>} ->
            gen_tcp:send(AcceptSocket,
                         list_to_binary("rooms:" ++ "Rooms: " ++ room_names())),
            handle_messages(Server, AcceptSocket);
        {tcp, _, _} ->
            gen_tcp:send(AcceptSocket,
                         prattle_utils:system_message("Invalid command sent. Use join:channel "
                                                      "or room:list"))
    end.

%% We are not starting room proccesses manually,
%% room supervisor does that for us. After
%% room creation we return its port.
create_room(Name) ->
    {ok, Pid} = supervisor:start_child(prattle_room_sup,
                                       [list_to_atom(Name)]),
    gen_server:call(Pid, {room_port, list_to_atom(Name)}).

room_names() ->
    Rooms = supervisor:which_children(prattle_room_sup),
    lists:map(fun ({_, RoomPid, _, _}) ->
                      atom_to_list(gen_server:call(RoomPid, name)) ++ " "
              end,
              Rooms).

%% Get room list from supervisor and pass them down for search.
room_port(Room) ->
    Rooms = supervisor:which_children(prattle_room_sup),
    room_port(Room, Rooms).

%% We are iterating room list, and if any will return integer
%% instead of "none" atom, we return it immediately.
room_port(Room, []) -> none;
room_port(Room, [Next | Rooms]) ->
    {_, Pid, _, _} = Next,
    Port = gen_server:call(Pid,
                           {room_port, list_to_atom(Room)}),
    if Port == none -> room_port(Room, Rooms);
       true -> Port
    end.

%% Helper utils
log(Message) -> log(Message, []).

log(Message, Args) ->
    io:format("[SERVER] " ++ Message ++ "~n", Args).
