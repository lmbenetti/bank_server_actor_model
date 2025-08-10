% raup@itu.dk * 2024-11-14

-module(account).

%% export any necessary functions
-export([start/0, start_reg/1, init/1]).

%% define the account actor state
-record(account_state, {accountname, balance}).

%% define a start(...) function that spawns an account actor
start() ->
    spawn(?MODULE, init, []).

start_reg(AccountName) ->
    PID = spawn(?MODULE, init, [AccountName]),
    register(AccountName, PID).

%% define a init(...) function that initalizes the state of the account actor
init(AccountName) ->
    State = #account_state{accountname = AccountName, balance = 100},
    loop(State).

%% loop(...) function with the behavior of the account actor upon receiving messages
loop(State) ->
    receive
        {deposit, {Amount}} -> 
            NewBalance = State#account_state.balance + Amount,
            NewState = State#account_state{balance = NewBalance},
        loop(NewState);
        print_balance ->
            io:format("The balance is ~p~n",
                      [State#account_state.balance]),
            loop(State)
    end.