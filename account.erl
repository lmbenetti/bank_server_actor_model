-module(account).
-export([start/0, start_reg/1, init/1]).
-record(account_state, {accountname, balance}).

%% Function that spawns an account actor
start() ->
    spawn(?MODULE, init, []).

%% Function that spawns and registers an account actor under the given name
start_reg(AccountName) ->
    PID = spawn(?MODULE, init, [AccountName]),
    register(AccountName, PID).

%% Function that initalizes the state of the account actor
init(AccountName) ->
    State = #account_state{accountname = AccountName, balance = 100},
    loop(State).

%% Function with the behavior of the account actor upon receiving messages
loop(State) ->
    receive
        {deposit, Amount} -> 
            NewBalance = State#account_state.balance + Amount,
            NewState = State#account_state{balance = NewBalance},
        loop(NewState);
        print_balance ->
            io:format("The balance is ~p~n",
                    [State#account_state.balance]),
            loop(State)
    end.