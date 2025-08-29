-module(mobile_app).
-export([start/3, start_reg/3, init/3, handle_n_requests/4]).
-record(mobile_app_state, {account, username, bankID}).

%% function that spawns a mobile app actor
start(AccountID, Username, BankID) ->
    spawn(?MODULE, init, [AccountID, Username, BankID]).

start_reg(AccountID, Username, BankID) ->
    PID = spawn(?MODULE, init, [AccountID, Username, BankID]),
    register(Username,PID).
    

%% function that initalizes the state of the mobile app actor
init(AccountID, Username, BankID) ->
    State = #mobile_app_state{account = AccountID, username = Username, bankID = BankID},
    State#mobile_app_state.bankID ! {new_account, self(), State#mobile_app_state.account},
    loop(State).


%% function with the behavior of the mobile app actor upon receiving messages
loop(State) ->
    receive
        {payment_request, SourceAccount, TargetAccount, Amount} -> 
            State#mobile_app_state.bankID ! {transaction, SourceAccount, TargetAccount, Amount, self()},
        loop(State);
        {payment_failed_balance, TargetAccount, Amount, BankName} -> 
            payment_failed_balance_handler(TargetAccount, Amount, BankName), 
        loop(State);
        {payment_failed_amount, TargetAccount, Amount} -> 
            payment_failed_amount_handler(TargetAccount, Amount), 
        loop(State);
        print_balance -> 
            State#mobile_app_state.account !{print_balance_with_owner, State#mobile_app_state.username},
        loop(State);
        {send_N_requests, AccountA1, AccountA2, N} ->
            handle_n_requests(AccountA1, AccountA2, N, State),
        loop(State)
        
    end.

%% function that handles N payment requests of 1
handle_n_requests(AccountA1, AccountA2, N, State) ->
        case N == 0 of
            true -> 
                io:format("Transactions are done!~n");
            false ->
                State#mobile_app_state.bankID ! {transaction, AccountA1, AccountA2, 1, self()},
                handle_n_requests(AccountA1, AccountA2, (N-1), State)
        end.

%% function that informs the user that a payment has failed due to insuficient balance
payment_failed_balance_handler(TargetAccount, Amount, BankID)->
    io:format("The bank ~p has informed that the transaction to account ~p for $ ~p, has failed due to insuficient balance~n",
                        [BankID, TargetAccount, Amount]).

%% function that informs the user that a payment has failed due to insuficient balance
payment_failed_amount_handler(TargetAccount, Amount)->
    io:format("The transaction to account ~p for $ ~p, has failed. The amount has to be bigger than 0~n",
                        [TargetAccount, Amount]).

