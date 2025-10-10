%Accounts were taken out of the server. Persons where added

-module(server).
-import(lists,[member/2]).
-export([start/1, start_reg/1, init/1, loop/1]).
-record(server_state, {server_name, mobile_app_list, people_list, bank_list, sent_to_bank_transactions, completed_transactions, failed_transactions, last_transaction_number, started}).

%% Function that spawns an account actor
start(ServerName) ->
    spawn(?MODULE, init, [ServerName]).

%% Function that spawns and registers an account actor under the given name
start_reg(ServerName) ->
    PID = spawn(?MODULE, init, [ServerName]),
    register(ServerName, PID),
    ServerName.

%% Function that initalizes the state of the server actor
init(ServerName) ->
    State = #server_state{server_name = ServerName, mobile_app_list = #{}, people_list = [], bank_list = [], sent_to_bank_transactions =#{}, completed_transactions = #{}, failed_transactions = #{}, last_transaction_number=0, started = false},
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
        {make_payment, MobileAppSource, MobileAppTarget, Amount} ->
            NewState = make_payment(State, MobileAppSource, MobileAppTarget, Amount),
            loop(NewState);
        {new_mobile_app, MobileAppID} ->
            NewState = new_mobile_app_handler(State, MobileAppID),
            loop(NewState);
        {person_added_to_app, MobileAppID, PersonID} ->
            NewState = person_added_to_app_handler(State, MobileAppID, PersonID),
            loop(NewState);
        {bank_added_to_app, MobileAppID, BankName, AccountNumber} ->
            NewState = bank_added_to_app_handler(State, MobileAppID, BankName, AccountNumber),
            loop(NewState);
        print_banks_list ->
            print_bank_list(State),
            loop(State);
        print_people_list ->
            print_people_list(State),
            loop(State);
        print_mobileapp_list ->
            print_mobile_app_list(State),
            loop(State);
        print_sent_to_bank_transactions ->
            print_sent_to_bank_transactions(State),
            loop(State);
        print_failed_transactions ->
            print_failed_transactions(State),
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
    case State#server_state.mobile_app_list == #{} of
        true -> 
                io:format("The server has no mobile apps registered~n");
        false ->
                io:format("The server has the following mobile apps: ~p~n",
                    [State#server_state.mobile_app_list])
    end.

% Function that request payments between mobile apps
make_payment(State, MobileAppSource, MobileAppTarget, Amount) ->
    case Amount < 1 of
        true -> 
            MobileAppSource ! {payment_failed_amount, MobileAppTarget, Amount},
            State;
        false ->
            MobileAppList = State#server_state.mobile_app_list,
            NewTransactionNumber = State#server_state.last_transaction_number + 1,
            notify_mobile_apps(MobileAppSource,MobileAppTarget,Amount,NewTransactionNumber),
            case {mobile_app_is_in_list(MobileAppSource,MobileAppList), mobile_app_is_in_list(MobileAppTarget,MobileAppList)} of
                {false, false} ->
                    NewState = source_and_target_not_registered(State, MobileAppSource, MobileAppTarget, NewTransactionNumber, Amount),
                    NewState;
                {false, true} ->
                    NewState = source_not_registered(State, MobileAppSource, MobileAppTarget, NewTransactionNumber, Amount),
                    NewState;
                {true, false} ->
                    NewState = target_not_registered(State, MobileAppSource, MobileAppTarget, NewTransactionNumber, Amount),
                    NewState;
                {true, true} ->
                    NewState = apps_registered(State, MobileAppSource, MobileAppTarget, Amount, NewTransactionNumber),
                    NewState
            end
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

print_failed_transactions(State) ->
    Map = State#server_state.failed_transactions,
    case Map =:= #{} of
        true ->
            io:format("There are not failed transactions.~n");
        false ->
            io:format("The following transactions have failed.~n"),
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

new_mobile_app_handler(State, MobileAppID) ->
    MobileAppList = State#server_state.mobile_app_list,
    case maps:is_key(MobileAppID, MobileAppList) of
        true ->
            io:format("Error. App ~p is already registered in the server ~n", [MobileAppID]),
            State;
        false -> 
            NewMobileApp = #{
                person => undefined,
                bank => undefined,
                account => undefined,
                verified => false
            },
            UpdatedMobileAppList = MobileAppList#{MobileAppID => NewMobileApp},
            NewState = State#server_state{mobile_app_list = UpdatedMobileAppList},
            loop(NewState)
    end.

person_added_to_app_handler(State, MobileAppID, PersonID) ->
    MobileAppList = State#server_state.mobile_app_list,
    RegisteredMobileApp = maps:get(MobileAppID,MobileAppList, undefined),
    case RegisteredMobileApp of
        undefined ->
            MobileAppID ! {app_not_registered_in_server, PersonID},
            State;
        _ ->
            UpdatedMobileApp = mobile_app_update(RegisteredMobileApp, PersonID),
            UpdatedMobileAppList = MobileAppList#{MobileAppID => UpdatedMobileApp},
            State#server_state{mobile_app_list = UpdatedMobileAppList}
    end.

bank_added_to_app_handler(State, MobileAppID, BankName, AccountNumber) ->
    MobileAppList = State#server_state.mobile_app_list,
    RegisteredMobileApp = maps:get(MobileAppID,MobileAppList, undefined),
    case RegisteredMobileApp of
        undefined ->
            MobileAppID ! {app_not_registered_in_server, BankName},
            State;
        _ ->
            UpdatedMobileApp = mobile_app_update(RegisteredMobileApp, BankName, AccountNumber),
            UpdatedMobileAppList = MobileAppList#{MobileAppID => UpdatedMobileApp},
            State#server_state{mobile_app_list = UpdatedMobileAppList}
    end.


mobile_app_update(MobileApp, DataToUpdate) ->
    UpdateMobileApp = MobileApp#{person => DataToUpdate},
    mobile_app_verified_updater(UpdateMobileApp).

mobile_app_update(MobileApp, DataToUpdate, AccountNumber) ->
    UpdateMobileApp = MobileApp#{bank => DataToUpdate, account => AccountNumber},
    mobile_app_verified_updater(UpdateMobileApp).


mobile_app_verified_updater(MobileApp) ->
    Bank   = maps:get(bank, MobileApp),
    Person = maps:get(person, MobileApp),
    case (Bank == undefined orelse Person == undefined) of
        true ->
            MobileApp;
        false ->
            MobileApp#{verified => true}
    end.



new_transaction(MobileAppSource,MobileAppTarget,Amount) -> 
    Transaction = #{
                source => MobileAppSource,
                target => MobileAppTarget,
                source_verified => false,
                target_verified => false,
                apps_verified => false,
                successful => false,
                amount => Amount
            },
    case {mobile_app_is_verified(MobileAppSource),mobile_app_is_verified(MobileAppTarget)} of
        {false, false} ->
            Transaction;
        {false, true} ->
            Transaction#{reason => "Source App was missing a bank or a person",target_verified => true};
        {true, false} ->
            Transaction#{reason => "Target App was missing a bank or a person", source_verified => true};
        {true, true} ->
            Transaction#{source_verified => true, target_verified => true, apps_verified => true}
    end.



notify_mobile_apps(MobileAppSource,MobileAppTarget,Amount,NewTransactionNumber) ->
    MobileAppSource ! {transaction_received_by_server, MobileAppSource, MobileAppTarget, 0, Amount, NewTransactionNumber},
    MobileAppTarget ! {transaction_received_by_server, MobileAppSource, MobileAppTarget, 1, Amount, NewTransactionNumber}.

mobile_app_is_in_list(MobileAppId, MobileAppList)->
    maps:is_key(MobileAppId, MobileAppList).

mobile_app_is_verified(MobileAppId)->
    maps:get(verified, MobileAppId).

new_failed_transaction(MobileAppSource, MobileAppTarget, Amount, Reason) ->
    Transaction = #{
                source => MobileAppSource,
                target => MobileAppTarget,
                source_verified => false,
                target_verified => false,
                apps_verified => false,
                successful => false,
                amount => Amount,
                reason => Reason
            },
    Transaction.

source_and_target_not_registered(State, MobileAppSource, MobileAppTarget, NewTransactionNumber, Amount) ->
    MobileAppSource ! {payment_failed_non_registered_both, MobileAppSource, MobileAppTarget, NewTransactionNumber, Amount, "Source"},
    MobileAppTarget ! {payment_failed_non_registered_both, MobileAppSource, MobileAppTarget, NewTransactionNumber, Amount, "Target"},
    FailedTransaction = new_failed_transaction(MobileAppSource, MobileAppTarget,Amount, "None of the Apps is registered in the server"),
    FailedTransactions = State#server_state.failed_transactions,
    UpdatedFailedTransactions = FailedTransactions#{NewTransactionNumber => FailedTransaction},
    State#server_state{failed_transactions = UpdatedFailedTransactions, last_transaction_number = NewTransactionNumber}.

source_not_registered(State, MobileAppSource, MobileAppTarget, NewTransactionNumber, Amount)->
    MobileAppSource ! {payment_failed_non_registered_source, MobileAppTarget, NewTransactionNumber, Amount, 0},
    MobileAppTarget ! {payment_failed_non_registered_target, MobileAppSource, NewTransactionNumber, Amount, 0},
    FailedTransaction = new_failed_transaction(MobileAppSource, MobileAppTarget,Amount, "The source App is not registered in the server"),
    FailedTransactions = State#server_state.failed_transactions,
    UpdatedFailedTransactions = FailedTransactions#{NewTransactionNumber => FailedTransaction},
    State#server_state{failed_transactions = UpdatedFailedTransactions, last_transaction_number = NewTransactionNumber}.

target_not_registered(State, MobileAppSource, MobileAppTarget, NewTransactionNumber, Amount) ->
    MobileAppSource ! {payment_failed_non_registered_source, MobileAppTarget, NewTransactionNumber, Amount, 1},
                    MobileAppTarget ! {payment_failed_non_registered_target, MobileAppSource, NewTransactionNumber, Amount, 1},
                    FailedTransaction = new_failed_transaction(MobileAppSource, MobileAppTarget,Amount, "The target App is not registered in the server"),
                    FailedTransactions = State#server_state.failed_transactions,
                    UpdatedFailedTransactions = FailedTransactions#{NewTransactionNumber => FailedTransaction},
                    State#server_state{failed_transactions = UpdatedFailedTransactions, last_transaction_number = NewTransactionNumber}.





notifiy_mobile_apps_failed_registry(Transaction, NewTransactionNumber) ->
    MobileAppSource = maps:get(source, Transaction),
    MobileAppTarget = maps:get(target, Transaction),
    Amount = maps:get(amount, Transaction),
    case {maps:get(source_verified, Transaction), maps:get(target_verified, Transaction)} of
        {false, false} ->
            MobileAppSource ! {payment_failed_both, MobileAppSource, MobileAppTarget, NewTransactionNumber, Amount, "Source"},
            MobileAppTarget ! {payment_failed_both, MobileAppSource, MobileAppTarget, NewTransactionNumber, Amount, "Target"};
        {false, true} ->
            MobileAppSource ! {payment_failed_source, MobileAppTarget, NewTransactionNumber, Amount, 0},
            MobileAppTarget ! {payment_failed_target, MobileAppSource, NewTransactionNumber, Amount, 0};
        {true, false} ->
            MobileAppSource ! {payment_failed_source, MobileAppTarget, NewTransactionNumber, Amount, 1},
            MobileAppTarget ! {payment_failed_target, MobileAppSource, NewTransactionNumber, Amount, 1}
    end.

apps_registered(State, MobileAppSource, MobileAppTarget, Amount, NewTransactionNumber) ->
    MobileAppList = State#server_state.mobile_app_list,
    Transaction = new_transaction(maps:get(MobileAppSource,MobileAppList), maps:get(MobileAppTarget, MobileAppList), Amount),
    case maps:get(apps_verified, Transaction) of
        false ->
            FailedTransactions = State#server_state.failed_transactions,
            UpdatedFailedTransactions = FailedTransactions#{NewTransactionNumber => Transaction},
            notifiy_mobile_apps_failed_registry(Transaction, NewTransactionNumber),
            State#server_state{failed_transactions = UpdatedFailedTransactions, last_transaction_number = NewTransactionNumber};
        true ->
            SourceAccount = get_source_account(MobileAppSource, MobileAppList),
            BankOfSource = get_source_bank(MobileAppSource, MobileAppList),
            TargetAccount = get_target_account(MobileAppTarget, MobileAppList),
            Amount = maps:get(amount, Transaction),
            io:format("The Source account of ~p is ~p. The Target account of ~p is ~p. Is that an error? ~n", [MobileAppSource, SourceAccount, MobileAppTarget, TargetAccount]),
            BankOfSource ! {transaction, SourceAccount, TargetAccount, Amount, MobileAppSource},
            SentToBankTransactions = State#server_state.sent_to_bank_transactions,
            UpdatedSentToBankTransactions = SentToBankTransactions#{NewTransactionNumber => Transaction},
            State#server_state{sent_to_bank_transactions = UpdatedSentToBankTransactions, last_transaction_number = NewTransactionNumber}
    end.

get_source_account(MobileAppSource, MobileAppList) ->
    MobileApp = maps:get(MobileAppSource, MobileAppList),
    maps:get(account, MobileApp).

get_target_account(MobileAppTarget, MobileAppList) ->
    MobileApp = maps:get(MobileAppTarget, MobileAppList),
    maps:get(account, MobileApp).

get_source_bank(MobileAppSource, MobileAppList) ->
    MobileApp = maps:get(MobileAppSource, MobileAppList),
    maps:get(bank, MobileApp).


            
            %TODO. Send the payment request to the bank.

% TODO Adding a person and a bank to a Mobile app should be done trough a request TO the server
% Right now is a request to the app, but the server should handle it instead as a request. 