%%%-------------------------------------------------------------------
%% @doc Prattle cli
%% @end
%%%-------------------------------------------------------------------

-module(prattle_app).

-behaviour(application).

-export([start/0, start/1, start/2, stop/1]).

start() -> prattle_sup:start_link({8000}).

start(Args) -> prattle_sup:start_link(Args).

start(_StartType, Args) -> prattle_sup:start_link(Args).

stop(_State) -> ok.

%% internal functions

