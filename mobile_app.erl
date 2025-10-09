-module(mobile_app).
-export([start/1, start_reg/1, init/1]).
-record(mobile_app_state, {mobile_app_id,person_id, bank_id}).

%% function that spawns a mobile app actor
start(MobileAppID) ->
    PID = spawn(?MODULE, init, [MobileAppID]),
    server ! {new_mobile_app, MobileAppID},
    PID.

start_reg(MobileAppID) ->
    PID = start(MobileAppID),
    register(MobileAppID,PID),
    MobileAppID.
    

%% function that initalizes the state of the mobile app actor
init(MobileAppID) ->
    State = #mobile_app_state{mobile_app_id = MobileAppID, person_id = undefined, bank_id = undefined},
    loop(State).


%% function with the behavior of the mobile app actor upon receiving messages
loop(State) ->
    receive
        % {payment_request, SourceAccount, TargetAccount, Amount} -> 
        %     State#mobile_app_state.bankID ! {transaction, SourceAccount, TargetAccount, Amount, self()},
        % loop(State);
        {successful_incoming_transaction, MoibleAppSource, Amount} ->
            NewState = successful_incoming_transaction_handler(State, MoibleAppSource, Amount),
            loop(NewState);
        {transaction_received_by_server, MobileAppSource, MobileAppTarget, Role, Amount, Server, TransactionNumber} ->
            NewState = transaction_received_by_server_handler(State, MobileAppSource, MobileAppTarget, Role, Amount, Server,TransactionNumber),
            loop(NewState);
        {payment_failed_source, MobileAppTarget, TransactionNumber, Amount, RoleThatFailed} ->
            NewState = payment_failed_source_handler(State, MobileAppTarget, TransactionNumber, Amount, RoleThatFailed),
            loop(NewState);
        {payment_failed_target, MobileAppSource, TransactionNumber, Amount, RoleThatFailed} ->
            NewState = payment_failed_target_handler(State, MobileAppSource, TransactionNumber, Amount, RoleThatFailed),
            loop(NewState);
        {account_ownership_positive, Bank} ->
            NewState = account_ownership_positive_handler(State, Bank),
            loop(NewState);
        {account_ownership_negative, PersonID, Bank} ->
            NewState = account_ownership_negative_handler(State, PersonID, Bank),
            loop(NewState);
        {add_bank, BankID} ->
            NewState = add_bank_handler(State, BankID),
            loop(NewState);
        {add_person, PersonID}->
            NewState = add_person_handler(State, PersonID),
            loop(NewState);
        % {app_not_registered_in_server, PersonID} -> TODO
        print ->
            NewState = print_handler(State),
            loop(NewState);
        {payment_failed_balance, TargetAccount, Amount, BankName} -> 
            payment_failed_balance_handler(TargetAccount, Amount, BankName), 
        loop(State);
        {payment_failed_amount, TargetAccount, Amount} -> 
            payment_failed_amount_handler(TargetAccount, Amount), 
        loop(State)
    end.


%% function that informs the user that a payment has failed due to insuficient balance
payment_failed_balance_handler(TargetAccount, Amount, BankID)->
    io:format("The bank ~p has informed that the transaction to account ~p for $ ~p, has failed due to insuficient balance~n",
                        [BankID, TargetAccount, Amount]).

%% function that informs the user that a payment has failed due to insuficient balance
payment_failed_amount_handler(TargetAccount, Amount)->
    io:format("The transaction to account ~p for $ ~p, has failed. The amount has to be bigger than 0~n",
                        [TargetAccount, Amount]).

print_handler(State) ->
    case app_has_person(State#mobile_app_state.person_id) of
        false ->
            io:format("This Mobile App has no person registered yet.~n"),
            State;
        true ->
            print_person_name(State#mobile_app_state.person_id),
            case app_has_bank(State#mobile_app_state.bank_id) of
                false ->
                    io:format("This Mobile App is not connected to any bank yet.~n"),
                    State;
                true ->
                    print_bank_name(State#mobile_app_state.bank_id),
                    State
            end
    end.

add_person_handler(State, PersonID) ->
    case app_has_person(State#mobile_app_state.person_id) of
        true ->
            io:format("This app is already assigned to a person.~n"),
            State;
        false ->
            server ! {person_added_to_app, State#mobile_app_state.mobile_app_id , PersonID},
            State#mobile_app_state{person_id = PersonID}
    end.

add_bank_handler(State, BankID) ->
    case app_has_person(State#mobile_app_state.person_id) of
        false ->
            io:format("This Mobile App has no person registered yet. Register a person before adding a bank~n"),
            State;
        true ->
            case app_has_bank(State#mobile_app_state.bank_id) of
                true ->
                    io:format("This Mobile App is already connected to a bank~n"),
                    State;                    
                false ->
                    io:format("We asked ~p bank if the registered person holds an account with them. We will inform you with their response~n", [BankID]),
                    BankID ! {person_has_account, State#mobile_app_state.person_id, State#mobile_app_state.mobile_app_id},
                    State
            end
    end.


app_has_person(PersonField) ->
    PersonField /= undefined.
app_has_bank(BankField) ->
    BankField /= undefined.
print_person_name(Name) ->
    io:format("This Mobile App belongs to ~p.~n",
        [Name]).
print_bank_name(Bank) ->
    io:format("This Mobile App is connected to ~p Bank.~n",
        [Bank]).

account_ownership_positive_handler(State, Bank) ->
    io:format("With regards your request, ~p Bank has confirmed that you hold an account with them and now this app is connected with that account.~n",
        [Bank]),
    server ! {bank_added_to_app, State#mobile_app_state.mobile_app_id, Bank},
    State#mobile_app_state{bank_id = Bank}.


account_ownership_negative_handler(State, PersonID, Bank) ->
    io:format("With regards your request, ~p Bank has informed that ~p is does not have an account with them. Please try again.~n",
        [Bank, PersonID]),
    State.

transaction_received_by_server_handler(State, MobileAppSource, MobileAppTarget, Role, Amount, Server, TransactionNumber) ->
    case Role of
        0 ->  
            io:format("The server received your payment request to ~p for $ ~p and assigned the transaction number ~p. The server will inform you about the result.~n",
            [MobileAppTarget, Amount, TransactionNumber]),
            Server ! {app_verification, State#mobile_app_state.mobile_app_id ,Role, TransactionNumber, (app_has_person(State#mobile_app_state.person_id) and app_has_bank(State#mobile_app_state.bank_id))},
            State;
        1 ->
            io:format("~p has initiated a payment to you for $ ~p under transaction number ~p. The server will inform you about the result.~n",
            [MobileAppSource, Amount, TransactionNumber]),
            Server ! {app_verification, State#mobile_app_state.mobile_app_id, Role, TransactionNumber, (app_has_person(State#mobile_app_state.person_id) and app_has_bank(State#mobile_app_state.bank_id))},
            State
    end.

successful_incoming_transaction_handler(State, MobileAppSource, Amount) ->
    io:format("You have received a transaction from ~p for $ ~p.~n",
        [MobileAppSource, Amount]),
    State.
payment_failed_source_handler(State, MobileAppTarget, TransactionNumber, Amount, RoleThatFailed) ->
    case RoleThatFailed of
        0 ->
            io:format("Your transaction to ~p for $ ~p under number ~p, has failed because your App is not linked to a person or a bank. Complete your App registration and try again. ~n",
            [MobileAppTarget, Amount, TransactionNumber]),
            State;
        1 ->
            io:format("Your transaction to ~p for $ ~p under number ~p, has failed because the recipient App is not linked to a person or a bank. Contact them to solve this issue and try again. ~n",
            [MobileAppTarget, Amount, TransactionNumber]),
            State
    end. 

payment_failed_target_handler(State, MobileAppSource, TransactionNumber, Amount, RoleThatFailed) ->
    case RoleThatFailed of
        1 ->
            io:format("The incoming transaction from ~p for $ ~p under number ~p, has failed because your App is not linked to a person or a bank. Complete your app registration and try again. ~n",
            [MobileAppSource, Amount, TransactionNumber]),
            State;
        0 -> 
            io:format("The incoming transaction from ~p for $ ~p under number ~p, has failed because their App is not linked to a person or a bank. Contact them to solve this issue and try again. ~n",
            [MobileAppSource, Amount, TransactionNumber]),
            State
    end.




