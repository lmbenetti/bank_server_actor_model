-module(bank).
-export([start/1, start_reg/1, init/1]).
-record(bank_state, {bank_name, accounts, last_account_number}).

%% function that spawns a bank actor
start(Bankname) ->
    spawn(?MODULE, init, [Bankname]).

start_reg(Bankname) ->
    PID = spawn(?MODULE, init, [Bankname]),
    register(Bankname, PID),
    PID.

%% function that initalizes the state of the bank actor
init(Bankname) ->
    State = #bank_state{bank_name = Bankname, accounts = #{}, last_account_number=1},
    loop(State).

%% function with the behavior of the bank actor upon receiving messages
loop(State) ->
    receive
        {new_account, Mobile_app_ID, AccountID} -> %To delete
            NewState = new_account_handler(State, Mobile_app_ID, AccountID),
            loop(NewState); 
        {open_account, PersonID} -> 
            NewState = open_account_handler(State,PersonID),
            loop(NewState);
        {transaction, SourceAccount, TargetAccount, Amount, Mobile_app_ID} ->
            transaction_handler(State, SourceAccount, TargetAccount, Amount, Mobile_app_ID),
            loop(State);
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
    Mobile_app_ID ! {payment_failed_balance, TargetAccount, Amount, State#bank_state.bank_name}.

%% Function that creates a new account in the bank's register
new_account_handler(State, Mobile_app_ID, AccountID) ->
    UpdatedMap = (State#bank_state.accounts)#{AccountID => Mobile_app_ID},
    NewState = State#bank_state{accounts = UpdatedMap},
    NewState.


%% Function that creates a new account in the bank's register, linked to a person
open_account_handler(State, PersonID) ->
    case has_an_account(State#bank_state.accounts,PersonID) of
        true -> 
            io:format("This person already has an account in this bank~n"),
            State;
        false ->
            NewAccountName = list_to_atom("account_" ++ integer_to_list(State#bank_state.last_account_number)), 
            UpdatedLastAccountNumber = State#bank_state.last_account_number +1,
            NewAccount = account:start_reg(NewAccountName),
            UpdatedMap = (State#bank_state.accounts)#{PersonID => NewAccount}, 
            State#bank_state{accounts = UpdatedMap, last_account_number = UpdatedLastAccountNumber}
    end. 

has_an_account(Accounts, PersonID) ->
    maps:is_key(PersonID, Accounts).

%% Function that handles a transaction, checking if the Mobile App owns the account
transaction_handler(State, SourceAccount, TargetAccount, Amount, Mobile_app_ID)->
    case maps:is_key(SourceAccount, State#bank_state.accounts) of
                false  -> 
                    io:format("The sender account is not registered in ~p Bank~n", [State#bank_state.bank_name]);
                true ->
                    Value = maps:get(SourceAccount, State#bank_state.accounts),
                    case Value == Mobile_app_ID of
                        true ->
                            SourceAccount ! {transaction, self(), Mobile_app_ID, TargetAccount, Amount};
                        false -> 
                            io:format("The sender account is not owned by the user ~n")
                    end
            end.

