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

-record(state, {socket, room_sup}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init({Socket, RoomSup}) ->
    gen_server:cast(self(), accept),
    {ok, #state{socket = Socket, room_sup = RoomSup}}.

stop(Name) -> gen_server:call(Name, stop).

handle_call(_E, _From, State) -> {noreply, State}.

handle_cast(accept,
            State = #state{socket = ListenSocket,
                           room_sup = RoomSupervisor}) ->
    run(ListenSocket, RoomSupervisor),
    {noreply, State}.

run(ListenSocket, RoomSupervisor) ->
    % Listen for messages or for single connection message
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    inet:setopts(AcceptSocket, [{active, once}]),
    listen(AcceptSocket, RoomSupervisor),
    gen_tcp:close(AcceptSocket),
    % Restart and wait for next connection
    run(ListenSocket, RoomSupervisor).

listen(AcceptSocket, RoomSupervisor) ->
    receive
        {tcp, AcceptSocket, <<"join", Name/binary>>} ->
            Rooms = supervisor:which_children(RoomSupervisor),
            Room = find_room(Name, Rooms),
            case Room of
                none ->
                    create_room(Name, RoomSupervisor),
                    join_room(Room, AcceptSocket);
                _ -> join_room(Room, AcceptSocket)
            end;
        {room, change, {Source, Dest}} ->
            Rooms = supervisor:which_children(RoomSupervisor),
            Room = find_room(Dest, Rooms),
            case Room of
                none ->
                    Room = create_room(Dest, RoomSupervisor),
                    Source ! {info, room_port, get_port(Room)};
                _ -> join_room(Room, AcceptSocket)
            end
    end.

create_room(Name, RoomSupervisor) ->
    {ok, Room} = supervisor:start_child(RoomSupervisor,
                                        Name),
    Room.

join_room(Room, AcceptSocket) ->
    Port = get_port(Room),
    gen_tcp:send(AcceptSocket,
                 <<"room_port", Port/binary>>).

get_port(Room) ->
    Room ! {request, port},
    receive
        {response, Port} -> Port;
        _ -> get_port(Room)
    end.

find_room(K, [H | T]) ->
    case H of
        {K, Pid, _, _} -> Pid;
        _ -> find_room(K, T)
    end;
find_room(_, []) -> none.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason,
          State = #state{socket = ListenSocket}) ->
    gen_tcp:close(ListenSocket),
    {ok, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
