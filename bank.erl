-module(bank).
-export([start/1, start_reg/1, init/1, loop/1]).
-record(bank_state, {bankname, accounts}).

%% function that spawns a bank actor
start(Bankname) ->
    spawn(?MODULE, init, [Bankname]).

start_reg(Bankname) ->
    PID = spawn(?MODULE, init, [Bankname]),
    register(Bankname, PID).

%% function that initalizes the state of the bank actor
init(Bankname) ->
    State = #bank_state{bankname = Bankname, accounts = #{}},
    loop(State).

%% function with the behavior of the bank actor upon receiving messages
loop(State) ->
    receive
        {newAccount, Mobile_app_ID, Account} -> 
            UpdatedMap = (State#bank_state.accounts)#{Account => Mobile_app_ID},
            NewState = State#bank_state{accounts = UpdatedMap},
            loop(NewState);

        {transaction, SourceAccount, TargetAccount, Amount, Mobile_app_ID} ->
            case maps:is_key(SourceAccount, State#bank_state.accounts) of
                false  -> 
                    io:format("The sender account is not registered in ~p Bank~n", [State#bank_state.bankname]),
                    loop(State);
                true ->
                    Value = maps:get(SourceAccount, State#bank_state.accounts),
                    case Value == Mobile_app_ID of
                        true ->
                            SourceAccount ! {transaction, self(), Mobile_app_ID, TargetAccount, Amount},
                            loop(State);
                        false -> 
                            io:format("The sender account is not owned by the user ~n"),
                            loop(State)
                    end
            end;
        {payment_failed, Mobile_app_ID, TargetAccount, Amount} -> 
            payment_failed_handler(State, Mobile_app_ID, TargetAccount, Amount), 
            loop(State);
        print_accounts ->
                io:format("The bank has this accounts ~p~n",
                        [State#bank_state.accounts]),
                loop(State)
    end.

%% Functiont that send the Mobile App a message, letting it know that a payment has failed.
payment_failed_handler(State, Mobile_app_ID, TargetAccount, Amount) ->
    Mobile_app_ID ! {payment_failed, TargetAccount, Amount, State#bank_state.bankname}.
