%%%-------------------------------------------------------------------
%% @doc Main prattle chat distribution hub
%% @end
%%%-------------------------------------------------------------------

-module(prattle_serv).

-behaviour(gen_server).

%% API
-export([start/1, start_link/1, stop/1]).

-export([code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         terminate/2]).

-record(state, {dummy}).

start({ListenSocket, RoomSup}) -> 
    io:format("Started prattle main server ~n"),
    { ok, AcceptSocket } = gen_tcp:accept(ListenSocket),
    io:format("server: Client connected ~n"),
    pong(AcceptSocket).

pong(Sock) -> 
    timer:sleep(3000),
    io:format("Sending pong ~n"),
    gen_tcp:send(Sock, <<"Pong">>),
    pong(Sock). 

stop(Name) -> gen_server:call(Name, stop).

start_link(Args) ->
    start(Args).

init(_Args) -> {ok, #state{dummy = 1}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
