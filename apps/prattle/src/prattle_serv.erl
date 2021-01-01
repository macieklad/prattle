%%%-------------------------------------------------------------------
%% @doc Main prattle chat distribution hub
%% @end
%%%-------------------------------------------------------------------

-module(prattle_serv).

-behaviour(gen_server).

%% API
-export([start_link/1, stop/1]).

-export([code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         terminate/2]).

-export([handle_connection/2, listen/2]).

-record(state, {socket}).

start_link(ListenSocket) ->
    io:format("[SERVER] starting prattle~n"),
    {ok, Serv} = gen_server:start_link({local, ?MODULE},
                                       ?MODULE,
                                       [ListenSocket],
                                       []),
    gen_server:cast(Serv, listen),
    {ok, Serv}.

init([ListenSocket]) ->
    {ok, #state{socket = ListenSocket}}.

stop(_Args) -> gen_server:call(self(), stop).

handle_call({room_port, Room}, _From, State) ->
    Port = room_port(Room),
    if Port == none -> {reply, create_room(Room), State};
       true -> {reply, Port, State}
    end.

handle_cast(listen,
            State = #state{socket = ListenSocket}) ->
    listen(self(), ListenSocket),
    io:format("[SERVER] Server listening, awaiting "
              "on connections ~n"),
    {noreply, State}.

listen(Server, ListenSocket) ->
    spawn_link(?MODULE,
               handle_connection,
               [Server, ListenSocket]).

handle_connection(Server, ListenSocket) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    listen(Server, ListenSocket),
    handle_messages(Server, AcceptSocket),
    gen_tcp:close(AcceptSocket).

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
                         list_to_binary("rooms:" ++ room_names())),
            handle_messages(Server, AcceptSocket);
        {tcp, _, Message} ->
            erlang:display(Message),
            gen_tcp:send(AcceptSocket,
                         "[ERROR] Invalid command sent. Use join:channe"
                         "l, disconnecting")
    end.

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

room_port(Room) ->
    Rooms = supervisor:which_children(prattle_room_sup),
    room_port(Room, Rooms).

room_port(Room, []) -> none;
room_port(Room, [Next | Rooms]) ->
    {_, Pid, _, _} = Next,
    Port = gen_server:call(Pid,
                           {room_port, list_to_atom(Room)}),
    if Port == none -> room_port(Room, Rooms);
       true -> Port
    end.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason,
          State = #state{socket = ListenSocket}) ->
    gen_tcp:close(ListenSocket),
    {ok, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
