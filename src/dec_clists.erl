% Tuple lists where each element has a 'count' value, implemented with
% decimals.
%
% Herman Bosma


%% TODO: proper terminology..

-module(dec_clists).

-export([eat/3,
         eat/2,
        
         count/1
        ]).

eat(Q, List) ->
    eat(Q, List, []).

%% Returns {ok, Eaten, Rest} | {hungry, QLeft, Rest}
eat(Q, [], Acc) ->
    {hungry, Q, Acc};
eat(Q, [{C, Val} | T], Acc) ->
    case dec:cmp(Q, C) of
        -1 ->
            Leave = dec:sub(C, Q),
            {ok, [{Q, Val} | Acc], [{Leave, Val} | T]};
        0 ->
            {ok, [{Q, Val} | Acc], T};
        1 ->
            Q1 = dec:sub(Q, C),
            eat(Q1, T, [{C, Val} | Acc])
    end.


count(List) ->
    count(List, 0).

count([], Total) ->
    Total;
count([{C, _Val} | T], Total) ->
    count(T, dec:add(C, Total)).
