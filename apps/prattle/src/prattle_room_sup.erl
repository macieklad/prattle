%%%-------------------------------------------------------------------
%% @doc Chat room supervisor, manages room creation and listing requests
%% @end
%%%-------------------------------------------------------------------

-module(prattle_room_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

-export([init/1]).

start_link() ->
    io:format("[ROOM SUPERVISOR]: Started ~n"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    SupFlags = #{strategy =>
                     simple_one_for_one, % one_for_one | one_for_all | rest_for_one | simple_one_for_one
                 intensity => 10, period => 60},
    ChildSpecs = [{chat_room,
                   {prattle_room, start_link, []},
                   temporary,
                   1000,
                   worker,
                   [prattle_room]}],
    {ok, {SupFlags, ChildSpecs}}.
