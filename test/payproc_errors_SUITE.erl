-module(payproc_errors_SUITE).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_payment_processing_errors_thrift.hrl").

-export([all/0]).

-export([known_error_test/1]).
-export([unknown_error_test/1]).
-export([unknown_error_atom_test/1]).
-export([bad_static_type_test/1]).
-export([formating_test/1]).

%%

-type config() :: hg_ct_helper:config().
-type test_case_name() :: hg_ct_helper:test_case_name().

-spec all() -> [test_case_name()].
all() ->
    [
        known_error_test,
        unknown_error_test,
        unknown_error_atom_test,
        bad_static_type_test,
        formating_test
    ].

-spec known_error_test(config()) -> ok.
known_error_test(_C) ->
    DE = #domain_Failure{
        code = <<"authorization_failed">>,
        sub = #domain_SubFailure{
            code = <<"account_limit_exceeded">>,
            sub = #domain_SubFailure{
                code = <<"amount">>,
                sub = #domain_SubFailure{
                    code = <<"monthly">>
                }
            }
        }
    },
    SE =
        {authorization_failed,
            {account_limit_exceeded, {amount, {monthly, #payprocerr_GeneralFailure{}}}}},
    DE = payproc_errors:construct('PaymentFailure', SE),
    ok = payproc_errors:match('PaymentFailure', DE, fun(E) when SE =:= E -> ok end),
    DE = payproc_errors:construct('RefundFailure', SE),
    ok = payproc_errors:match('RefundFailure', DE, fun(E) when SE =:= E -> ok end).

-spec unknown_error_atom_test(config()) -> ok.
unknown_error_atom_test(_C) ->
    UnknownCode = <<"unknown big fucking error">>,
    DE = #domain_Failure{
        code = UnknownCode
    },
    SE = {{unknown_error, UnknownCode}, #payprocerr_GeneralFailure{}},
    DE = payproc_errors:construct('PaymentFailure', SE),
    ok = payproc_errors:match('PaymentFailure', DE, fun(E) when SE =:= E -> ok end).

-spec unknown_error_test(config()) -> ok.
unknown_error_test(_C) ->
    UnknownCode = erlang:atom_to_binary(bad_error_code, utf8),
    DE = #domain_Failure{
        code = UnknownCode
    },
    SE = {{unknown_error, UnknownCode}, #payprocerr_GeneralFailure{}},
    DE = payproc_errors:construct('PaymentFailure', SE),
    ok = payproc_errors:match('PaymentFailure', DE, fun(E) when SE =:= E -> ok end).

-spec bad_static_type_test(config()) -> ok.
bad_static_type_test(_C) ->
    Bad = {qwe, #payprocerr_GeneralFailure{}},
    {'EXIT', {badarg, _}} =
        (catch payproc_errors:construct('PaymentFailure', {authorization_failed, Bad})),
    {'EXIT', {badarg, _}} =
        (catch payproc_errors:construct('PaymentFailure', {qwe, Bad})),
    {'EXIT', {badarg, _}} =
        (catch payproc_errors:construct('PaymentFailure', Bad)),
    {'EXIT', {badarg, _}} =
        (catch payproc_errors:construct('RefundFailure', {terms_violated, Bad})),
    {'EXIT', {badarg, _}} =
        (catch payproc_errors:construct('RefundFailure', {preauthorization_failed, #payprocerr_GeneralFailure{}})),
    {'EXIT', {badarg, _}} =
        (catch payproc_errors:construct('RefundFailure', Bad)),
    ok.

-spec formating_test(config()) -> ok.
formating_test(_C) ->
    SE =
        {authorization_failed,
            {payment_tool_rejected, {bank_card_rejected, {cvv_invalid, #payprocerr_GeneralFailure{}}}}},
    Type = 'PaymentFailure',
    <<"authorization_failed:payment_tool_rejected:bank_card_rejected:cvv_invalid">> =
        erlang:list_to_binary(payproc_errors:format(Type, payproc_errors:construct(Type, SE))).
