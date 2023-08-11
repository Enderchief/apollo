-module(ffi_apollo).
-export([validate/2, typed/2]).

validate(X, Y) ->
    X(Y).

do_check(Value, Checks) ->
    lists:filtermap(
        fun(Check) ->
            case Check(Value) of
                {ok, nil} -> false;
                {error, Msg} -> {true, Msg}
            end
        end,
        Checks
    ).

-spec valid_type(Type :: binary(), Value :: apollo_t:value()) -> boolean().
valid_type(Type, Value) ->
    case Type of
        <<"Bool">> -> is_boolean(Value);
        <<"Float">> -> is_float(Value);
        <<"Int">> -> is_integer(Value);
        <<"String">> -> is_binary(Value);
        _ -> false
    end.

do_find_check(List) ->
    fun({K, V}) ->
        case lists:keyfind(K, 1, List) of
            false ->
                {true,
                    unicode:characters_to_binary([
                        <<"Invalid property name '">>, K, <<"'">>
                    ])};
            {_, {value, Type, Checks}} ->
                case valid_type(Type, V) of
                    false ->
                        {true,
                            unicode:characters_to_binary(
                                [<<"Type of value `">>, "_", <<"` is not `">>, Type, <<"`">>]
                            )};
                    true ->
                        N = do_check(V, Checks),

                        case length(N) of
                            0 ->
                                false;
                            _ ->
                                [First | _] = N,
                                {true, First}
                        end
                end
        end
    end.

typed(Constructor, List) ->
    fun(Args) ->
        Errored =
            (lists:filtermap(
                do_find_check(List),
                tuple_to_list(Args)
            )),

        case length(Errored) of
            0 ->
                {ok,
                    apply(
                        Constructor,
                        lists:map(
                            fun({_, V}) -> V end, tuple_to_list(Args)
                        )
                    )};
            _ ->
                [First | _] = Errored,
                {error, First}
        end
    end.
