%%%-------------------------------------------------------------------
%% @doc Single chat room server
%% @end
%%%-------------------------------------------------------------------

-module(prattle_room).

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

start(Name) -> io:format("Chat room status"), timer:sleep(500), start(Name).

stop(Name) -> gen_server:call(Name, stop).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

init(_Args) -> {ok, #state{dummy = 1}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
