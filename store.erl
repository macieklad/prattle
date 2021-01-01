-module(store).

-export([start/1]).

-export([put/2, run/1, take/1]).

start(StateList) ->
    register(store,
             spawn_link(?MODULE, run, [dict:from_list(StateList)])).

run(State) ->
    receive
        {take, Key, Process} ->
            Process ! {return, dict:fetch(State, Key)};
        {store, {Key, Value}} ->
            run(dict:store(Key, Value, State))
    end,
    run(State).

take(Key) ->
    store ! {take, Key, self()},
    receive {return, Value} -> Value end.

put(Key, Value) -> store ! {store, {Key, Value}}.
