-module(prattle_store).

-export([start/1]).

-export([put/2, run/1, take/1]).

start(StateList) ->
    Pid = spawn_link(?MODULE,
                     run,
                     [dict:from_list(StateList)]),
    register(store_instance, Pid).

run(State) ->
    receive
        {take, Key, Process} ->
            Process ! {return, dict:fetch(Key, State)};
        {put, {Key, Value}} ->
            run(dict:store(Key, Value, State))
    end,
    run(State).

take(Key) ->
    store_instance ! {take, Key, self()},
    receive {return, Value} -> Value end.

put(Key, Value) -> store_instance ! {put, {Key, Value}}.
