%Accounts were taken out of the server. Persons where added

-module(server).
-import(lists,[member/2]).
-export([start/0, start_reg/1, init/1, loop/1]).
-record(server_state, {server_name, mobile_app_list, people_list, bank_list, started}).

%% Function that spawns an account actor
start() ->
    spawn(?MODULE, init, []).

%% Function that spawns and registers an account actor under the given name
start_reg(ServerName) ->
    PID = spawn(?MODULE, init, [ServerName]),
    register(ServerName, PID).

%% Function that initalizes the state of the server actor
init(ServerName) ->
    State = #server_state{server_name = ServerName, mobile_app_list = [], people_list = [], bank_list = [], started = false},
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
        print_people_list ->
            print_people_list(State),
            loop(State);
        print_mobileapp_list ->
            print_mobile_app_list(State),
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
add_mobile_app(State,AccountID,BankID,UserID) ->
    case member(UserID, State#server_state.mobile_app_list) of
        true -> 
                io:format("The user ~p already has an app~n ",
                        [UserID]),
                State;
        false -> 
                mobile_app:start_reg(AccountID,UserID,BankID),
                NewCreatedApps = [UserID | State#server_state.mobile_app_list],
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
make_payment(MobileAppSender, AccountIDSender, AccountIDReceiver, Amount) ->
    case Amount < 1 of
        true -> 
            MobileAppSender ! {payment_failed_amount, AccountIDReceiver, Amount};
        false ->
            MobileAppSender ! {payment_request, AccountIDSender, AccountIDReceiver, Amount}
    end.



