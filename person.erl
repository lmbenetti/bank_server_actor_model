-module(person).
-export([start/1, start_reg/2, init/1]).
-record(person_state, {name}).

%% function that spawns a person actor
start(GivenName) ->
    spawn(?MODULE, init, [GivenName]).

start_reg(PersonID,GivenName) ->
    PID = spawn(?MODULE, init, [GivenName]),
    register(PersonID, PID),
    PersonID.

%% function that initalizes the state of the person actor
init(GivenName) ->
    State = #person_state{name = GivenName},
    loop(State).

loop(State) ->
    receive
        print_name ->
            io:format("The person is called: ~p~n",[State#person_state.name]),
            loop(State)
        end.


