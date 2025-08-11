-module(interface).
-export([start/0, make_payment/0, checker/0]).

%% Function that starts two mobile_apps with its accounts related to a bank
start() ->
    account:start_reg(a1),
    account:start_reg(a2),
    bank:start_reg(danske),
    bank:start_reg(jyske),
    mobile_app:start_reg(a1, peter, danske),
    mobile_app:start_reg(a2, lisandro, danske).

%% Function that makes payments between mobile apps
make_payment() ->
    peter ! {payment_request, a1,a2,20},
    lisandro ! {payment_request, a2,a1,10}.

%% This exercise shows that the prints happens before the 100 transactions are done.
%% This shows that the model actor works, but it can have delays while displaying information correctly.
checker() ->
    peter ! {send_N_requests, a1, a2, 100},
    a1 ! print_balance,
    a2 ! print_balance.