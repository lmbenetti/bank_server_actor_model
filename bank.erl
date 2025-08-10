% raup@itu.dk * 2024-11-14

-module(bank).

%% export any necessary functions
-export([start/1, start_reg/1, init/1, loop/1]).

%% define the bank actor state
-record(bank_state, {bankname, accounts}).

%% define a start(...) function that spawns a bank actor
start(Bankname) ->
    spawn(?MODULE, init, [Bankname]).

start_reg(Bankname) ->
    PID = spawn(?MODULE, init, [Bankname]),
    register(Bankname, PID).

%% define a init(...) function that initalizes the state of the bank actor
init(Bankname) ->
    State = #bank_state{bankname = Bankname, accounts = #{}},
    loop(State).

%% loop(...) function with the behavior of the bank actor upon receiving messages
loop(State) ->
    receive
        {newAccount, {Mobile_app_ID, Account}} -> 
            UpdatedMap = (State#bank_state.accounts)#{Account => Mobile_app_ID},
            NewState = State#bank_state{accounts = UpdatedMap},
        loop(NewState);

        {transaction, {AccountA1, AccountA2, Amount,Mobile_app_ID}} ->
            case maps:is_key(AccountA1, State#bank_state.accounts) of
                false  -> 
                    io:format("The sender account is not registered in ~p Bank~n", [State#bank_state.bankname]),
                    loop(State);
                true ->
                    Value = maps:get(AccountA1, State#bank_state.accounts),
                    case Value == Mobile_app_ID of
                        true -> 
                            MinusAmount = Amount * -1,
                            AccountA1 ! {deposit, {MinusAmount}},
                            AccountA2 ! {deposit, {Amount}},
                            loop(State);
                        false -> 
                            io:format("The sender account is not owned by the user ~n"),
                            loop(State)
                    end
            end;
        print_accounts ->
                io:format("The bank has this accounts ~p~n",
                        [State#bank_state.accounts]),
                loop(State)
    end.