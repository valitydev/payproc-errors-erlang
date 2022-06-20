-module(payproc_errors_SUITE).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_payproc_error_thrift.hrl").

-export([all/0]).

-export([known_error_test/1]).
-export([unknown_error_test/1]).
-export([unknown_error_atom_test/1]).
-export([bad_static_type_test/1]).
-export([formatting_test/1]).

%%

-type config() :: [{atom(), term()}].
-type test_case_name() :: atom().

-spec all() -> [test_case_name()].
all() ->
    [
        known_error_test,
        unknown_error_test,
        unknown_error_atom_test,
        bad_static_type_test,
        formatting_test
    ].

-spec known_error_test(config()) -> _.
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
    SE = {authorization_failed, {account_limit_exceeded, {amount, {monthly, #payproc_error_GeneralFailure{}}}}},
    DE = payproc_errors:construct('PaymentFailure', SE),
    ok = payproc_errors:match('PaymentFailure', DE, fun(E) when SE =:= E -> ok end),
    DE = payproc_errors:construct('RefundFailure', SE),
    ok = payproc_errors:match('RefundFailure', DE, fun(E) when SE =:= E -> ok end).

-spec unknown_error_atom_test(config()) -> _.
unknown_error_atom_test(_C) ->
    UnknownCode = <<"unknown big fucking error">>,
    DE = #domain_Failure{
        code = UnknownCode
    },
    SE = {{unknown_error, UnknownCode}, #payproc_error_GeneralFailure{}},
    DE = payproc_errors:construct('PaymentFailure', SE),
    ok = payproc_errors:match('PaymentFailure', DE, fun(E) when SE =:= E -> ok end).

-spec unknown_error_test(config()) -> _.
unknown_error_test(_C) ->
    UnknownCode = erlang:atom_to_binary(bad_error_code, utf8),
    DE = #domain_Failure{
        code = UnknownCode
    },
    SE = {{unknown_error, UnknownCode}, #payproc_error_GeneralFailure{}},
    DE = payproc_errors:construct('PaymentFailure', SE),
    ok = payproc_errors:match('PaymentFailure', DE, fun(E) when SE =:= E -> ok end).

-spec bad_static_type_test(config()) -> _.
bad_static_type_test(_C) ->
    Bad = {qwe, #payproc_error_GeneralFailure{}},
    {'EXIT', {badarg, _}} =
        (catch payproc_errors:construct('PaymentFailure', {authorization_failed, Bad})),
    {'EXIT', {badarg, _}} =
        (catch payproc_errors:construct('PaymentFailure', {qwe, Bad})),
    {'EXIT', {badarg, _}} =
        (catch payproc_errors:construct('PaymentFailure', Bad)),
    {'EXIT', {badarg, _}} =
        (catch payproc_errors:construct('RefundFailure', {terms_violated, Bad})),
    {'EXIT', {badarg, _}} =
        (catch payproc_errors:construct('RefundFailure', {preauthorization_failed, #payproc_error_GeneralFailure{}})),
    {'EXIT', {badarg, _}} =
        (catch payproc_errors:construct('RefundFailure', Bad)),
    ok.

-spec formatting_test(config()) -> _.
formatting_test(_C) ->
    SE =
        {authorization_failed,
            {payment_tool_rejected, {bank_card_rejected, {cvv_invalid, #payproc_error_GeneralFailure{}}}}},
    Type = 'PaymentFailure',
    <<"authorization_failed:payment_tool_rejected:bank_card_rejected:cvv_invalid">> =
        erlang:list_to_binary(payproc_errors:format(Type, payproc_errors:construct(Type, SE))).
