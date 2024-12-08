-module(day3).

-export([solve/1]).


-include_lib("eunit/include/eunit.hrl").

solve(File) ->
  {ok, Bin} = file:read_file(File),
  {solve_p1(Bin), solve_p2(Bin)}.

solve_p1(Bin) ->
  {ok, RE} = re:compile("mul\\((\\d+),(\\d+)\\)"),
  {match, Captures} = re:run(Bin, RE, [global, {capture, all_but_first, binary}]),
  lists:foldl(fun([X, Y], Acc) ->
                  Acc + binary_to_integer(X) * binary_to_integer(Y)
              end, 0, Captures).

solve_p2(Bin) ->
  {ok, RE} = re:compile("(mul\\((\\d+),(\\d+)\\)|do\\(\\)|don't\\(\\))"),
  {match, Captures} = re:run(Bin, RE, [global, {capture, all_but_first, binary}]),
  io:format("~p~n", [Captures]),
  {_, P2} =
    lists:foldl(fun([_, X, Y], {true, Acc}) ->
                    {true, Acc + binary_to_integer(X) * binary_to_integer(Y)};
                   ([<<"mul(", _/binary>>, _, _], {false, _} = Acc) ->
                    Acc;
                   ([<<"do()">>], {_, Acc}) ->
                    {true, Acc};
                   ([<<"don't()">>], {_, Acc}) ->
                    {false, Acc}
                end, {true, 0}, Captures),
  P2.


ex1_test() ->
  Bin = <<"xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))">>,
  ?assertEqual(161, solve_p1(Bin)).

ex2_test() ->
  Bin = <<"xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))">>,
  ?assertEqual(48, solve_p2(Bin)).
