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
        {transaction, BankID, Mobile_app_ID, TargetAccount, Amount} ->
            NewState = transaction_handler(State,BankID, Mobile_app_ID, TargetAccount, Amount),
            loop(NewState);
        {deposit, Amount} -> 
            NewState = deposit_hanlder(State,Amount),
            loop(NewState);
        print_balance ->
            io:format("The balance is ~p~n",
                    [State#account_state.balance]),
            loop(State);
        {print_balance_with_owner, MobileAppID} ->
            print_balance_with_owner(State, MobileAppID),
            loop(State)
    end.


%% Function that prints the balance and a user name
print_balance_with_owner(State, MobileAppID) ->
    io:format("The balance of ~p is ~p ~n",
                    [MobileAppID, State#account_state.balance ]).

%% Function that handles trasactions. It checks that the balance is enough to pay the request
transaction_handler(State,BankID,Mobile_app_ID, TargetAccount, Amount) ->
    NewBalance = State#account_state.balance - Amount,
    case NewBalance < 0 of
            true ->
                BankID ! {payment_failed, Mobile_app_ID, TargetAccount, Amount},
                State;
            false-> 
                NewState = State#account_state{balance = NewBalance},
                TargetAccount ! {deposit, Amount},
                NewState
    end.

%% Function that handles deposits
deposit_hanlder(State, Amount) ->
    NewBalance = State#account_state.balance + Amount,
    NewState = State#account_state{balance = NewBalance},
    NewState.

                





