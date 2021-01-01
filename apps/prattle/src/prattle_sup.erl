%%%-------------------------------------------------------------------
%% @doc Prattle top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(prattle_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Args) ->
    supervisor:start_link({local, ?SERVER},
                          ?MODULE,
                          [Args]).

init([ServerArgs]) ->
    SupFlags = #{strategy => one_for_one, intensity => 0,
                 period => 1},
    ChildSpecs = [#{id => room_sup,
                    start => {prattle_room_sup, start_link, []},
                    restart => permanent, shutdown => 5000,
                    type => supervisor, modules => [prattle_room_sup]},
                  #{id => serv,
                    start => {prattle_serv, start_link, [ServerArgs]},
                    restart => permanent, shutdown => 5000, type => worker,
                    modules => [prattle_serv]}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

