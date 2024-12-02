-module(day2).

-export([solve/1,
         solve0/1, is_safe_incr/1]).

-include_lib("eunit/include/eunit.hrl").

solve(Input) ->
  {ok, Bin} = file:read_file(Input),
  solve0(Bin).

solve0(Bin) ->
  Lines = binary:split(Bin, <<"\n">>, [global]),
  P1 =
    lists:foldl(fun(<<>>, Acc) -> Acc;
                   (Line, {Acc0, Acc1}) ->
                    Report = lists:map(fun binary_to_integer/1, binary:split(Line, <<" ">>, [global])),
                    {Acc0 + is_safe(Report), Acc1 + is_safe2(Report)}
                end, {0, 0}, Lines),
  P1.

%% Part 1

is_safe([A, B|_] = Report) when A < B ->
  is_safe_incr(Report);
is_safe([A, B|_] = Report) when A > B ->
  is_safe_decr(Report);
is_safe(_) ->
  0.

is_safe_incr([_]) -> 1;
is_safe_incr([A, B|Rest]) when B - A >= 1 andalso B - A =< 3 ->
  is_safe_incr([B|Rest]);
is_safe_incr(_) ->
  0.

is_safe_decr([_]) -> 1;
is_safe_decr([A, B|Rest]) when A - B >= 1 andalso A - B =< 3 ->
  is_safe_decr([B|Rest]);
is_safe_decr(_) ->
  0.

%% Part 2

%% Brute-force this to avoid various corner cases by generating all
%% lists where we delete one item at a time and check if the resulting
%% list is safe.
is_safe2(Report) ->
  SkipIndexes = lists:seq(0, length(Report) - 1),
  case lists:any(fun(L) -> is_safe(L) == 1 end,
                 lists:map(fun(Idx) ->
                               delete_at(Idx, Report)
                           end, SkipIndexes)) of
    true -> 1;
    false -> 0
  end.

delete_at(Idx, List) ->
  {L1, [_|L2]} = lists:split(Idx, List),
  L1 ++ L2.

-ifdef(TEST).
ex1_test() ->
  ?assertEqual(1, is_safe([7,6,4,2,1])),
  ?assertEqual(0, is_safe([1,2,7,8,9])),
  ?assertEqual(0, is_safe([9,7,6,2,1])),
  ?assertEqual(0, is_safe([1,3,2,4,5])),
  ?assertEqual(0, is_safe([8,6,4,4,1])),
  ?assertEqual(1, is_safe([1,3,6,7,9])).

ex2_test() ->
  ?assertEqual(1, is_safe2([7,6,4,2,1])),
  ?assertEqual(0, is_safe2([1,2,7,8,9])),
  ?assertEqual(0, is_safe2([9,7,6,2,1])),
  ?assertEqual(1, is_safe2([1,3,2,4,5])),
  ?assertEqual(1, is_safe2([8,6,4,4,1])),
  ?assertEqual(1, is_safe2([1,3,6,7,9])).


-endif.
