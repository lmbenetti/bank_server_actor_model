-module(system_starter).
-export([system_start2/0, make_payment/0, checker/0]).

system_start2() ->
    account:start_reg(a1),
    account:start_reg(a2),
    bank:start_reg(danske),
    bank:start_reg(jyske),
    mobile_app:start_reg(a1, peter, danske),
    mobile_app:start_reg(a2, lisandro, danske).

make_payment() ->
    peter ! {payment_request, {a1,a2,20}},
    lisandro ! {payment_request, {a2,a1,10}}.

checker() ->
    peter ! {send_N_requests, {a1, a2, 100}},
    a1 ! print_balance,
    a2 ! print_balance.