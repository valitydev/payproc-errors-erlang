-module(payproc_errors_SUITE).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_payproc_error_thrift.hrl").

-export([all/0]).

-export([known_error_test/1]).
-export([unknown_error_atom_test/1]).
-export([unknown_error_nested_test/1]).
-export([bad_static_type_test/1]).
-export([formatting_test/1]).
-export([from_notation_test/1]).
-export([to_notation_test/1]).
-export([match_notation_test/1]).

%%

-type config() :: [{atom(), term()}].
-type test_case_name() :: atom().

-spec all() -> [test_case_name()].
all() ->
    [
        known_error_test,
        unknown_error_atom_test,
        unknown_error_nested_test,
        bad_static_type_test,
        formatting_test,
        from_notation_test,
        to_notation_test,
        match_notation_test
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

-spec unknown_error_nested_test(config()) -> _.
unknown_error_nested_test(_C) ->
    DE = #domain_Failure{
        code = <<"no_route_found">>,
        sub = #domain_SubFailure{
            code = <<"forbidden">>,
            sub = #domain_SubFailure{
                code = <<"rejected_routes">>,
                sub = #domain_SubFailure{
                    code = <<"limit_hold_reject">>
                }
            }
        }
    },
    SE =
        {no_route_found,
            {
                {unknown_error, <<"forbidden">>},
                {
                    {unknown_error, <<"rejected_routes">>},
                    {{unknown_error, <<"limit_hold_reject">>}, #payproc_error_GeneralFailure{}}
                }
            }},
    EquivalentSE =
        {no_route_found,
            {forbidden,
                {
                    {unknown_error, <<"rejected_routes">>},
                    {{unknown_error, <<"limit_hold_reject">>}, #payproc_error_GeneralFailure{}}
                }}},
    DE = payproc_errors:construct('PaymentFailure', SE),
    ok = payproc_errors:match('PaymentFailure', DE, fun(E) when EquivalentSE =:= E -> ok end).

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

-spec from_notation_test(config()) -> _.
from_notation_test(_C) ->
    #domain_Failure{
        code = <<"failure">>,
        reason = <<"Failure reason">>,
        sub = #domain_SubFailure{
            code = <<"sub_failure1">>,
            sub = #domain_SubFailure{
                code = <<"sub_failure2">>
            }
        }
    } =
        payproc_errors:from_notation(<<"failure:sub_failure1:sub_failure2">>, <<"Failure reason">>),

    #domain_Failure{
        code = <<"failure">>,
        reason = <<"Failure reason">>
    } =
        payproc_errors:from_notation(<<"failure">>, <<"Failure reason">>),

    undefined = payproc_errors:from_notation(<<"">>, <<"Failure reason">>).

-spec to_notation_test(config()) -> _.
to_notation_test(_C) ->
    <<"failure:sub_failure1:sub_failure2">> = payproc_errors:to_notation(#domain_Failure{
        code = <<"failure">>,
        reason = <<"Failure reason">>,
        sub = #domain_SubFailure{
            code = <<"sub_failure1">>,
            sub = #domain_SubFailure{
                code = <<"sub_failure2">>
            }
        }
    }),

    <<"failure">> = payproc_errors:to_notation(#domain_Failure{
        code = <<"failure">>,
        reason = <<"Failure reason">>
    }).

-spec match_notation_test(config()) -> _.
match_notation_test(_C) ->
    Failure = #domain_Failure{
        code = <<"failure">>,
        reason = <<"Failure reason">>,
        sub = #domain_SubFailure{
            code = <<"sub_failure1">>,
            sub = #domain_SubFailure{
                code = <<"sub_failure2">>
            }
        }
    },
    ok = payproc_errors:match_notation(Failure, fun(<<"failure:sub_failure1:sub_failure2">>) -> ok end),
    ok = payproc_errors:match_notation(Failure, fun(<<"failure:sub_failure1", _/binary>>) -> ok end).
