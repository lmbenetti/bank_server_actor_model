% raup@itu.dk * 2024-11-14

-module(mobile_app).

%% export any necessary functions
-export([start/3, start_reg/3, init/3, handle_n_requests/4]).

%% define the mobile app actor state
-record(mobile_app_state, {account, username, bankID}).

%% define a start(...) function that spawns a mobile app actor
start(AccountID, Username, BankID) ->
    spawn(?MODULE, init, [AccountID, Username, BankID]).

start_reg(AccountID, Username, BankID) ->
    PID = spawn(?MODULE, init, [AccountID, Username, BankID]),
    register(Username,PID).
    

%% define a init(...) function that initalizes the state of the mobile app actor
init(AccountID, Username, BankID) ->

    State = #mobile_app_state{account = AccountID, username = Username, bankID = BankID},
    State#mobile_app_state.bankID ! {newAccount, {self(), State#mobile_app_state.account}},
    loop(State).


%% loop(...) function with the behavior of the mobile app actor upon receiving messages
loop(State) ->
    receive
        {payment_request, {AccountA1, AccountA2, Amount}} -> 
            State#mobile_app_state.bankID ! {transaction, {AccountA1, AccountA2, Amount, self()}},
        loop(State);
        {send_N_requests, {AccountA1, AccountA2, N}} ->
            handle_n_requests(AccountA1, AccountA2, N, State),
        loop(State)
        
    end.

handle_n_requests(AccountA1, AccountA2, N, State) ->
        case N == 0 of
            true -> 
                io:format("Transactions are done!~n");
            false ->
                State#mobile_app_state.bankID ! {transaction, {AccountA1, AccountA2, 1, self()}},
                handle_n_requests(AccountA1, AccountA2, (N-1), State)
        end.

