%%%-------------------------------------------------------------------
%% @doc Single chat room server
%% @end
%%%-------------------------------------------------------------------

-module(prattle_room).

-behaviour(gen_server).

%% API
-export([start_link/1, stop/1]).

-export([code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         terminate/2]).

-export([client/3]).

-record(state, {socket, port, clients}).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

% connect
% leave
% msg
init(_Args) ->
    {ok, ListenSocket} = gen_tcp:listen(0,
                                        [binary, {active, true}]),
    {ok, Port} = inet:port(ListenSocket),
    io:format("Listening on ~w ~n", [Port]),
    gen_server:cast(self(), accept),
    {ok,
     #state{socket = ListenSocket, port = Port,
            clients = []}}.

stop(_Args) -> ok.

handle_call(stop, _From, State) -> {noreply, State}.

handle_cast(accept,
            State = #state{socket = ListenSocket, port = Port}) ->
    io:format("Awaiting client connections: ~n"),
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    io:format("Client connection received ~n"),
    receive
        {tcp, AcceptSocket, <<"connect:", Name/binary>>} ->
            connect_client(Name, AcceptSocket),
            gen_server:cast(self, accept);
        _ ->
            gen_tcp:close(AcceptSocket),
            gen_server:cast(self, accept)
    end,
    {noreply, State}.

connect_client(Name, AcceptSocket) ->
    Pid = spawn_link(?MODULE,
                     client,
                     [Name, AcceptSocket, self()]),
    gen_tcp:send(AcceptSocket,
                 io_lib:format("Welcome to room: ~s ~n", [Name])),
    gen_tcp:controlling_process(AcceptSocket, Pid).

client(Name, Socket, Room) ->
    receive
        {tcp, Socket, <<"msg:", Message/binary>>} ->
            gen_tcp:send(Socket, Message);
        {tcp, _, Message} ->
            io:format("Received unrecognized command ~s ~n",
                      [binary_to_list(Message)])
    end,
    client(Name, Socket, Room).

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
