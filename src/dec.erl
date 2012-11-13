%%%-------------------------------------------------------------------
%%% @author Herman Bosma <hermanbosma@gmail.com>
%%% @copyright 2012 Herman Bosma
%%% @doc Erlang decimal type with simple arithmetic.
%%%      Conspicuouly absent is division; use mul instead.
%%% @end
%%%-------------------------------------------------------------------
-module(dec).

-export([make/1,
         show/1,
         reduce/1,
         neg/1,
         add/2,
         sub/2,
         mul/2,
         cmp/2,
         gt/2,
         gte/2,
         eq/2,
         lt/2,
         lte/2,
         is_zero/1]).

make({N, E}) ->
    {N, E};
make(N) when is_integer(N) ->
    {N, 0};
make(X) when is_list(X) ->
    % Note that this will accept "-10.-1". Don't do that.
    case lists:splitwith(fun(C) -> C =/= $. end, X) of
        {Whole, []} ->
            {list_to_integer2(Whole), 0};
        {Whole, [$., Digit | Frac]} when Digit >= $0 andalso Digit =< $9 ->
            MinE = length(Frac),
            N = list_to_integer2(Whole) * intpow(10, MinE) + case Whole of
                [$- | _] ->
                    -list_to_integer(Frac);
                _ ->
                    list_to_integer(Frac)
            end,
            {N, -MinE}
    end;
make(X) when is_binary(X) ->
    make(binary_to_list(X)).

show(X) ->
    {N, E} = reduce(X),
    L = integer_to_list(N),
    if
        E >= 0 ->
            L ++ lists:duplicate(E, $0);
        E < 0 ->
            CO = intpow(10, -E),
            NQ = N div CO, NR = N rem CO,
            LR = integer_to_list(NR),
            Leading0s = -(E + length(LR)), % Always good
            Padding = lists:duplicate(Leading0s, $0),
            integer_to_list(NQ) ++ [$. | Padding] ++ LR
    end.


reduce(X) ->
    {N, E} = make(X),
    case {N, N rem 10} of
        {NonZero, 0} when NonZero =/= 0 ->
            reduce({N div 10, E + 1});
        _ ->
            {N, E}
    end.


is_zero(X) ->
    {N, _E} = make(X),
    N =:= 0.

neg(X) ->
    {N, E} = make(X),
    {-N, E}.

add(X, Y) ->
    {{NX, E}, {NY, E}} = level(X, Y),
    {NX + NY, E}.

sub(X, Y) ->
    add(X, neg(Y)).

mul(X, Y) ->
    {NX, EX} = make(X),
    {NY, EY} = make(Y),
    {NX * NY, EX + EY}.

cmp(X, Y) ->
    {{NX, E}, {NY, E}} = level(X, Y),
    S = NX + NY,
    if
        S > 0 -> 1;
        S =:= 0 -> 0;
        S < 0 -> -1
    end.

gt(X, Y) ->
    {{NX, E}, {NY, E}} = level(X, Y),
    NX > NY.

gte(X, Y) ->
    {{NX, E}, {NY, E}} = level(X, Y),
    NX >= NY.

eq(X, Y) ->
    {{NX, E}, {NY, E}} = level(X, Y),
    NX =:= NY.

lt(X, Y) ->
    {{NX, E}, {NY, E}} = level(X, Y),
    NX < NY.

lte(X, Y) ->
    {{NX, E}, {NY, E}} = level(X, Y),
    NX =< NY.


%%%===================================================================
%%% Internal functions
%%%===================================================================
% Return  numbers with the same exponent. Does not reduce.
level({NX, EX}, {NY, EY}) when EX >= EY ->
    {{NX * intpow(10, EX - EY), EY}, {NY, EY}};
level({NX, EX}, {NY, EY}) ->
    {{NX, EX}, {NY * intpow(10, EY - EX), EX}};
level(X, Y) ->
    level(make(X), make(Y)).


intpow(B, E) ->
    intpow(B, E, 1).

intpow(_, E, _) when E < 0 -> error("intpow: Negative exponent ");
intpow(_, 0, _) -> 1;
intpow(B, 1, Acc) -> B * Acc;
intpow(B, E, Acc) ->
    case E band 1 of
        0 -> intpow(B * B, E bsr 1, Acc);
        _ -> intpow(B, E - 1, B * Acc)
    end.

% Allow empty list to be zero
list_to_integer2([]) ->
    0;
list_to_integer2(X) ->
    list_to_integer(X).
