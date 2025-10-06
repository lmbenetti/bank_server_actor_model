%Accounts were taken out of the server. Persons where added

-module(server).
-import(lists,[member/2]).
-export([start/0, start_reg/1, init/1, loop/1]).
-record(server_state, {server_name, mobile_app_list, people_list, bank_list, pending_to_verify_transactions, sent_to_bank_transactions, completed_transactions,last_transaction_number, started}).

%% Function that spawns an account actor
start() ->
    spawn(?MODULE, init, []).

%% Function that spawns and registers an account actor under the given name
start_reg(ServerName) ->
    PID = spawn(?MODULE, init, [ServerName]),
    register(ServerName, PID),
    ServerName.

%% Function that initalizes the state of the server actor
init(ServerName) ->
    State = #server_state{server_name = ServerName, mobile_app_list = [], people_list = [], bank_list = [], pending_to_verify_transactions = #{}, sent_to_bank_transactions =#{}, completed_transactions = #{}, last_transaction_number=0, started = false},
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
        {add_person, PersonID, PersonName} ->
            NewState = add_person(State, PersonID, PersonName),
            loop(NewState);
        {add_mobileapp, AccountID, BankID, MobileAppID} ->
            NewState = add_mobile_app(State,AccountID,BankID,MobileAppID),
            loop(NewState);
        {make_payment, MobileAppSource, MobileAppTarget, Amount} ->
            NewState = make_payment(State, MobileAppSource, MobileAppTarget, Amount),
            loop(NewState);
        {app_verification, MobileAppID, Role, TransactionNumber, Verified} ->
            NewState = app_verification_handler(State, MobileAppID, Role, TransactionNumber, Verified),
            loop(NewState);
        print_all_balances ->
            print_balances(State),
            loop(State);
        print_banks_list ->
            print_bank_list(State),
            loop(State);
        print_people_list ->
            print_people_list(State),
            loop(State);
        print_mobileapp_list ->
            print_mobile_app_list(State),
            loop(State);
        print_pending_to_verify_transactions ->
            print_pending_to_verify_transactions(State),
            loop(State);
        print_sent_to_bank_transactions ->
            print_sent_to_bank_transactions(State),
            loop(State);
        print_completed_transactions ->
            print_completed_transactions(State),
            loop(State)
    end.

%% Function that starts the system with prefilled data
start_model(State) ->
    case State#server_state.started of 
        true -> 
                io:format("The server has already been initialized~n"),
                State;
        false ->
                Banks = [danske, jyske, al, nordea, lunar],
                Persons = [
                    {lisandro, "Lisandro Marco Benetti"},
                    {marcus, "Marcus Pedersen"},
                    {marie, "Marie Halst"},
                    {valdemar, "Valdemar Jensen"},
                    {signe, "Signe Taliones"},
                    {olivia, "Olivia Hansen"},
                    {noah, "Noah Klasz"}
                    
                ],
                Apps = [app1, app2, app3, app4, app5, app6, app7, app8],
                PersonIDs = [person:start_reg(ID, Name) || {ID, Name} <- Persons],
                lists:foreach(fun(Bank) -> bank:start_reg(Bank) end, Banks),
                lists:foreach(fun(App) -> mobile_app:start_reg(App) end, Apps),
                danske ! {open_account, lisandro}, app1 ! {add_person, lisandro}, app1 ! {add_bank, danske},
                danske ! {open_account, marcus}, app2 ! {add_person, marcus}, app2 ! {add_bank, danske},
                jyske ! {open_account, marie}, app3 ! {add_person, marie}, app3 ! {add_bank, jyske},
                al ! {open_account, valdemar}, app4 ! {add_person, valdemar}, app4 ! {add_bank, al},
                nordea ! {open_account, signe}, app5 ! {add_person, signe}, app5 ! {add_bank, nordea},
                lunar ! {open_account, olivia}, app6 ! {add_person, olivia}, app6 ! {add_bank, lunar},
                lunar ! {open_account, noah}, app7 ! {add_person, noah}, app7 ! {add_bank, lunar},
                NewState = State#server_state{
                                            mobile_app_list = Apps,
                                            people_list = PersonIDs,
                                            bank_list=Banks, 
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

%% Function that adds a person, if not yet added
add_person(State, PersonID, PersonName) ->
    case member(PersonID, State#server_state.people_list) of
        true -> 
            io:format("The server already has the person ~p ~n ",
                        [PersonID]),
                State;
        false ->
            person:start_reg(PersonID, PersonName),
            NewPersonList = [PersonID | State#server_state.people_list],
            State#server_state{people_list = NewPersonList}
    end.

%% Function that adds a mobileapp, if not yet added
add_mobile_app(State,AccountID,BankID,MobileAppID) ->
    case member(MobileAppID, State#server_state.mobile_app_list) of
        true -> 
                io:format("The user ~p already has an app~n ",
                        [MobileAppID]),
                State;
        false -> 
                mobile_app:start_reg(AccountID,MobileAppID,BankID),
                NewCreatedApps = [MobileAppID | State#server_state.mobile_app_list],
                NewState = State#server_state{mobile_app_list = NewCreatedApps, started=true},
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
print_people_list(State) ->
    case State#server_state.people_list == [] of
        true -> 
                io:format("The server has no people registered~n");
        false ->  
                io:format("The server has the following people:~n"),
                lists:foreach(fun(People) ->
                io:format(" - ~p~n", [People])
                end, State#server_state.people_list)
    end.

%% Function that prints the list of mobile apps registered in the server
print_mobile_app_list(State)->
    case State#server_state.mobile_app_list == [] of
        true -> 
                io:format("The server has no mobile apps registered~n");
        false ->
                io:format("The server has the following mobile apps:~n"),
                lists:foreach(fun(MobileAPP) ->
                io:format(" - ~p~n", [MobileAPP])
                end, State#server_state.mobile_app_list)
    end.

%% Function that prints the balance of all the mobile apps in the server
print_balances(State) ->
    case State#server_state.mobile_app_list == [] of
        true -> 
                io:format("The server has no mobile apps registered~n");
        false ->
                lists:foreach(fun(MobileAPP) ->
                MobileAPP ! print_balance
                end, State#server_state.mobile_app_list)
    end.



% Function that request payments between mobile apps
make_payment(State, MobileAppSource, MobileAppTarget, Amount) ->
    case Amount < 1 of
        true -> 
            MobileAppSource ! {payment_failed_amount, MobileAppTarget, Amount},
            State;
        false ->
            NewTransactionNumber = State#server_state.last_transaction_number + 1,
            Transaction = #{
                source => MobileAppSource,
                target => MobileAppTarget,
                source_approved => false,
                target_approved => false,
                apps_verified => false,
                successful => false,
                amount => Amount
            },
            Pending = State#server_state.pending_to_verify_transactions,
            UpdatedPendingTransactions = Pending#{NewTransactionNumber => Transaction},
            NewState = State#server_state{
                pending_to_verify_transactions = UpdatedPendingTransactions,
                last_transaction_number = NewTransactionNumber
                },
            MobileAppSource ! {transaction_received_by_server, MobileAppSource, MobileAppTarget, 0, Amount, State#server_state.server_name, NewTransactionNumber},
            MobileAppTarget ! {transaction_received_by_server, MobileAppSource, MobileAppTarget, 1, Amount, State#server_state.server_name, NewTransactionNumber},
            NewState
    end.

print_pending_to_verify_transactions(State) ->
    Map = State#server_state.pending_to_verify_transactions,
    case Map =:= #{} of
        true ->
            io:format("There are not transactions pending.~n");
        false ->
            io:format("The following transactions are pending.~n"),
            lists:foreach(
                fun({K, V}) ->
                    io:format("~p => ~p~n", [K, V])
                end,
                maps:to_list(Map))
    end.

print_sent_to_bank_transactions(State) ->
    Map = State#server_state.sent_to_bank_transactions,
    case Map =:= #{} of
        true ->
            io:format("There are not transactions sent to the bank.~n");
        false ->
            io:format("The following transactions were sent to the bank.~n"),
            lists:foreach(
                fun({K, V}) ->
                    io:format("~p => ~p~n", [K, V])
                end,
                maps:to_list(Map))
    end.

print_completed_transactions(State) ->
    Map = State#server_state.completed_transactions,
    case Map =:= #{} of
        true ->
            io:format("There are not transactions completed yet.~n");
        false ->
            io:format("The following transactions have been completed.~n"),
            lists:foreach(
                fun({K, V}) ->
                    io:format("~p => ~p~n", [K, V])
                end,
      maps:to_list(Map))
    end.

app_verification_handler(State, MobileAppID, Role, TransactionNumber, Verified) ->
    Pending = State#server_state.pending_to_verify_transactions,
    case is_key(TransactionNumber, Pending) of
        false ->
            io:format("Error: Mobile app ~p sent a verification response for the Transaction Number ~p but this transaction was not pending.~n",
        [MobileAppID, TransactionNumber]),
        State;
        true -> 
            Transaction = get(TransactionNumber, Pending),
            case get(source, Transaction) =:= MobileAppID of
                true ->
                    source_approved =>

                    
    


