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
                   (Line, Acc) ->
                    Report = lists:map(fun binary_to_integer/1, binary:split(Line, <<" ">>, [global])),
                    io:format(">> ~w ", [Report]),
                    case is_safe(Report) of
                      true ->
                        io:format(" [SAFE]~n", []),
                        1 + Acc;
                      _ ->
                        io:format("~n", []),
                        Acc
                    end
                end, 0, Lines),
  P1.

is_safe([A, B|_] = Report) when A < B ->
  is_safe_incr(Report);
is_safe([A, B|_] = Report) when A > B ->
  is_safe_decr(Report);
is_safe(_) ->
  false.

is_safe_incr([_]) -> true;
is_safe_incr([A, B|Rest]) when B - A >= 1 andalso B - A =< 3 ->
  is_safe_incr([B|Rest]);
is_safe_incr(_) ->
  false.

is_safe_decr([_]) -> true;
is_safe_decr([A, B|Rest]) when A - B >= 1 andalso A - B =< 3 ->
  is_safe_decr([B|Rest]);
is_safe_decr(_) ->
  false.


-ifdef(TEST).
is_safe_test() ->
  ?assert(is_safe_incr([1,2])),
  ?assert(is_safe([1,2,3])).

ex1_test() ->
  ?assert(is_safe([7,6,4,2,1])),
  ?assertNot(is_safe([1,2,7,8,9])),
  ?assertNot(is_safe([9,7,6,2,1])),
  ?assertNot(is_safe([1,3,2,4,5])),
  ?assertNot(is_safe([8,6,4,4,1])),
  ?assert(is_safe([1,3,6,7,9])).

-endif.
