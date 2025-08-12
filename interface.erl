-module(interface).
-import(lists,[member/2]).
-export([start/1, start_reg/1, init/1, loop/1]).
-record(interface_state, {interfacename,created_accounts, account_counter, created_apps, bank_list, started}).

%% Function that spawns an account actor
start(InterfaceName) ->
    spawn(?MODULE, init, []).

%% Function that spawns and registers an account actor under the given name
start_reg(InterfaceName) ->
    PID = spawn(?MODULE, init, [InterfaceName]),
    register(InterfaceName, PID).

%% Function that initalizes the state of the interface actor
init(InterfaceName) ->
    State = #interface_state{interfacename = InterfaceName, created_accounts = [], account_counter=1, created_apps = [], bank_list = [], started = false},
    loop(State).

%% Function with the behavior of the interface actor upon receiving messages
loop(State) ->
    receive
        easy_start ->
            NewState = start_model(State),
            loop(NewState);
        {add_bank, BankName} ->
            NewState = add_bank(State, BankName),
            loop(NewState);
        print_bank_list ->
            print_bank_list(State),
            loop(State)

            
            
    end.

%% Function that starts the system with prefilled data
start_model(State) ->
    case State#interface_state.started of 
        true -> io:format("The model has already been initialized~n"),
                State;
        false ->Banks = [danske, jyske, al, nordea, lunar],
                Accounts = [a1,a2,a3,a4,a5,a6],
                MobileApps = [lisandro, henrik, peter, marie, signe, valdemar],
                lists:foreach(fun(Bank) -> bank:start_reg(Bank) end, Banks),
                lists:foreach(fun(Acc) -> bank:start_reg(Acc) end, Accounts),
                mobile_app:start_reg(a1, lisandro, danske),
                mobile_app:start_reg(a2, henrik, danske),
                mobile_app:start_reg(a3, peter, jyske),
                mobile_app:start_reg(a4, marie, al),
                mobile_app:start_reg(a5, signe, nordea),
                mobile_app:start_reg(a6, valdemar, jyske),
                NewCreatedApps = MobileApps,
                NewBankList = Banks,
                NewCreatedAccounts = Accounts,
                NewState = State#interface_state{created_accounts = NewCreatedAccounts, 
                                            account_counter= length(NewCreatedAccounts),
                                            created_apps = NewCreatedApps, 
                                            bank_list=NewBankList, 
                                            started=true},
                NewState
    end.

add_bank(State, Bankname)->
    case member(Bankname, State#interface_state.bank_list) of
        true -> io:format("The interface already has the ~p bank~n ",
                        [Bankname]),
                State;
        false -> bank:start_reg(Bankname),
                NewBankList = [Bankname | State#interface_state.bank_list],
                NewState = State#interface_state{bank_list = NewBankList, started=true},
                NewState
    end.

print_bank_list(State)->
    case State#interface_state.bank_list == [] of
        true -> io:format("The interface has no banks registered~n");
        false ->  io:format("The interface has the following banks:~n"),
                lists:foreach(fun(Bank) ->
                io:format(" - ~p~n", [Bank])
                end, State#interface_state.bank_list)
    end.

%%start_account(number)->
%%start_accounts(number)->

% new_mobile_app() ->
%     hello.


%% Function that makes payments between mobile apps
% make_payment() ->
%     peter ! {payment_request, a1,a2,20},
%     lisandro ! {payment_request, a2,a1,10}.

%% This exercise shows that the prints happens before the 100 transactions are done.
%% This shows that the model actor works, but it can have delays while displaying information correctly.
% checker() ->
%     peter ! {send_N_requests, a1, a2, 100},
%     a1 ! print_balance,
%     a2 ! print_balance.

