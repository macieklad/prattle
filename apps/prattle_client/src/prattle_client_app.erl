%%%-------------------------------------------------------------------
%% @doc Chat client
%% @end
%%%-------------------------------------------------------------------

-module(prattle_client_app).

-behaviour(application).

-export([start/2, stop/1]).

start() -> start({{127, 0, 0, 1}, 8000}).

start(_Type, _Args) -> start().

start({ServerIp, ServerPort}) ->
    Conn = gen_tcp:connect(ServerIp,
                           ServerPort,
                           [binary, {active, true}]),
    case Conn of
        {ok, Socket} ->
            io:format("Entering lobby, set nickname first with "
                      "'nick:your_nick' and then connect to "
                      "room with join:room_name ~n"),
            lobby(Socket);
        {error, Reason} ->
            io:format("Could not connect to main chat server: "
                      "~s ~n",
                      [Reason]),
            io:format("Retrying in 5 seconds. ~n"),
            timer:sleep(5000),
            start({ServerIp, ServerPort})
    end.

stop(_State) -> ok.

lobby(Socket) ->
    receive
        {tcp, Socket, D} -> io:format("~s ~n", [D]);
        _ -> io:format("Other message")
    end,
    lobby(Socket).

chat() -> 1.
