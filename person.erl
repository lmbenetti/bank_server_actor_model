-module(person).
-export([start/1, start_reg/2, init/1]).
-record(person_state, {name}).

%% function that spawns a person actor
start(Name) ->
    spawn(?MODULE, init, [Name]).

start_reg(PersonName,Name) ->
    PID = spawn(?MODULE, init, [Name]),
    register(PersonName, PID),
    PID.

%% function that initalizes the state of the person actor
init(Name) ->
    State = #person_state{name = Name},
    loop(State).

loop(State) ->
    receive
        print_name ->
            io:format("The person is called: ~p~n",[State#person_state.name]),
            loop(State)
        end.


