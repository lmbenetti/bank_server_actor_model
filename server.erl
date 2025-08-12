-module(server).
-import(lists,[member/2]).
-export([start/0, start_reg/1, init/1, loop/1]).
-record(server_state, {server_name,created_accounts, created_apps, bank_list, started}).

%% Function that spawns an account actor
start() ->
    spawn(?MODULE, init, []).

%% Function that spawns and registers an account actor under the given name
start_reg(ServerName) ->
    PID = spawn(?MODULE, init, [ServerName]),
    register(ServerName, PID).

%% Function that initalizes the state of the server actor
init(ServerName) ->
    State = #server_state{server_name = ServerName, created_accounts = [], created_apps = [], bank_list = [], started = false},
    loop(State).

%% Function with the behavior of the server actor upon receiving messages
loop(State) ->
    receive
        easy_start ->
            NewState = start_model(State),
            loop(NewState);
        {add_bank, BankID} ->
            NewState = add_bank(State, BankID),
            loop(NewState);
        {add_account, AccountID} ->
            NewState = add_account(State, AccountID),
            loop(NewState);
        {add_mobileapp, AccountID, BankID, UserID} ->
            NewState = add_mobile_app(State,AccountID,BankID,UserID),
            loop(NewState);
        {make_payment, MobileAppSender, AccountIDSender, AccountIDReceiver, Amount} ->
            make_payment(MobileAppSender, AccountIDSender, AccountIDReceiver, Amount),
            loop(State);
        print_all_balances ->
            print_balances(State),
            loop(State);
        print_banks_list ->
            print_bank_list(State),
            loop(State);
        print_accounts_list ->
            print_account_list(State),
            loop(State);
        print_mobileapp_list ->
            print_mobile_app_list(State),
            loop(State)

    end.

%% Function that starts the system with prefilled data
start_model(State) ->
    case State#server_state.started of 
        true -> 
                io:format("The model has already been initialized~n"),
                State;
        false ->
                Banks = [danske, jyske, al, nordea, lunar],
                Accounts = [a1,a2,a3,a4,a5,a6],
                MobileApps = [lisandro, henrik, peter, marie, signe, valdemar],
                lists:foreach(fun(Bank) -> bank:start_reg(Bank) end, Banks),
                lists:foreach(fun(Acc) -> account:start_reg(Acc) end, Accounts),
                mobile_app:start_reg(a1, lisandro, danske),
                mobile_app:start_reg(a2, henrik, danske),
                mobile_app:start_reg(a3, peter, jyske),
                mobile_app:start_reg(a4, marie, al),
                mobile_app:start_reg(a5, signe, nordea),
                mobile_app:start_reg(a6, valdemar, jyske),
                NewCreatedApps = MobileApps,
                NewBankList = Banks,
                NewCreatedAccounts = Accounts,
                NewState = State#server_state{created_accounts = NewCreatedAccounts, 
                                            created_apps = NewCreatedApps, 
                                            bank_list=NewBankList, 
                                            started=true},
                NewState
    end.

%% Function that adds a bank, if not yet added
add_bank(State, BankID)->
    case member(BankID, State#server_state.bank_list) of
        true -> 
                io:format("The server already has the ~p bank~n ",
                        [BankID]),
                State;
        false -> 
                bank:start_reg(BankID),
                NewBankList = [BankID | State#server_state.bank_list],
                NewState = State#server_state{bank_list = NewBankList, started=true},
                NewState
    end.

%% Function that adds an account, if not yet added
add_account(State, AccountID)->
    case member(AccountID, State#server_state.created_accounts) of
        true -> 
                io:format("The server already has the ~p account~n ",
                        [AccountID]),
                State;
        false -> 
                account:start_reg(AccountID),
                NewCreatedAccounts = [AccountID | State#server_state.created_accounts],
                NewState = State#server_state{created_accounts = NewCreatedAccounts, started=true},
                NewState
    end.

%% Function that adds a mobileapp, if not yet added
add_mobile_app(State,AccountID,BankID,UserID) ->
    case member(UserID, State#server_state.created_apps) of
        true -> 
                io:format("The user ~p already has an app~n ",
                        [UserID]),
                State;
        false -> 
                mobile_app:start_reg(AccountID,UserID,BankID),
                NewCreatedApps = [UserID | State#server_state.created_apps],
                NewState = State#server_state{created_apps = NewCreatedApps, started=true},
                NewState
    end.

%% Function that prints the list of banks registered in the server
print_bank_list(State)->
    case State#server_state.bank_list == [] of
        true -> 
                io:format("The server has no banks registered~n");
        false ->  
                io:format("The server has the following banks:~n"),
                lists:foreach(fun(Bank) ->
                io:format(" - ~p~n", [Bank])
                end, State#server_state.bank_list)
    end.

%% Function that prints the list of accounts registered in the server
print_account_list(State)->
    case State#server_state.created_accounts == [] of
        true -> 
                io:format("The server has no accounts registered~n");
        false ->  
                io:format("The server has the following accounts:~n"),
                lists:foreach(fun(Account) ->
                io:format(" - ~p~n", [Account])
                end, State#server_state.created_accounts)
    end.

%% Function that prints the list of mobile apps registered in the server
print_mobile_app_list(State)->
    case State#server_state.created_apps == [] of
        true -> 
                io:format("The server has no mobile apps registered~n");
        false ->
                io:format("The server has the following mobile apps:~n"),
                lists:foreach(fun(MobileAPP) ->
                io:format(" - ~p~n", [MobileAPP])
                end, State#server_state.created_apps)
    end.

%% Function that prints the balance of all the mobile apps in the server
print_balances(State) ->
    case State#server_state.created_apps == [] of
        true -> 
                io:format("The server has no mobile apps registered~n");
        false ->
                lists:foreach(fun(MobileAPP) ->
                MobileAPP ! print_balance
                end, State#server_state.created_apps)
    end.


% Function that request payments between mobile apps
make_payment(MobileAppSender, AccountIDSender, AccountIDReceiver, Amount) ->
    MobileAppSender ! {payment_request, AccountIDSender, AccountIDReceiver, Amount}.



