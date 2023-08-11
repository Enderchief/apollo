-module(apollo_t).
-export([new_value/1, custom/2]).
-export_type([value/0]).

-type value() :: {value, atom(), list()}.

-spec new_value(Str :: binary()) -> value().
new_value(Str) ->
    {value, Str, []}.

-spec custom(V :: value(), Validator :: fun((any()) -> {ok, nil} | {error, binary()})) -> value().
custom({Value, Type, List}, Validator) ->
    {Value, Type,
        lists:append([
            List, [validation_factory(Type, Validator)]
        ])}.

validation_factory(Type, Validator) ->
    fun(O) ->
        case Type of
            <<"Bool">> -> Validator({bool_t, O});
            <<"Float">> -> Validator({float_t, O});
            <<"Int">> -> Validator({int_t, O});
            <<"String">> -> Validator({string_t, O});
            _ -> {error, <<"Something went wrong">>}
        end
    end.
