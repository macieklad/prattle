%%%-------------------------------------------------------------------
%% @doc Simple process which keeps client configuration inside dict
%% @end
%%%-------------------------------------------------------------------
-module(prattle_store).

-export([start/1]).

-export([put/2, run/1, take/1]).

%% Initialize the store with values. It uses dict underneath, so
%% we initalize it by passing list with key-value tuples
%% e.g. [{key, val}, {foo, bar}].
start(StateList) ->
    Pid = spawn_link(?MODULE,
                     run,
                     [dict:from_list(StateList)]),
    register(store_instance, Pid).

%% Store process will run infinitely and return the requested
%% configuration values in response to process messages.
run(State) ->
    receive
        {take, Key, Process} ->
            Process ! {return, dict:fetch(Key, State)};
        {put, {Key, Value}} ->
            run(dict:store(Key, Value, State))
    end,
    run(State).

%% Lastly it exposes abstraction methods to make
%% everything clean without using receives
%% all over the place.
take(Key) ->
    store_instance ! {take, Key, self()},
    receive {return, Value} -> Value end.

put(Key, Value) -> store_instance ! {put, {Key, Value}}.
