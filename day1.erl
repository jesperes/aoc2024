-module(day1).

-export([solve/1, bench/2]).

solve(Input) ->
  {ok, Bin} = file:read_file(Input),
  Parsed = parse(Bin, []),
  {solve_p1(Parsed),
   solve_p2(Parsed)}.

bench(Input, N) ->
  {ok, Bin} = file:read_file(Input),
  Parsed = parse(Bin, []),
  bench:bench(
    fun() ->
        solve_p1(Parsed),
        solve_p2(Parsed)
    end, N).

solve_p1(Parsed) ->
  {List1, List2} = lists:unzip(Parsed),
  lists:foldl(fun({A, B}, Acc) ->
                  abs(A - B) + Acc
              end, 0, lists:zip(lists:sort(List1), lists:sort(List2))).

%% The numbers are 5-digit decimal numbers separated by three spaces,
%% so do some speedy parsing using binary matching.
parse(<<>>, Acc) -> Acc;
parse(<<X1,X2,X3,X4,X5,32,32,32,Y1,Y2,Y3,Y4,Y5,$\n,Rest/binary>>, Acc) ->
  parse(Rest, [{(X1 - $0) * 10000 +
                  (X2 - $0) * 1000 +
                  (X3 - $0) * 100 +
                  (X4 - $0) * 10 +
                  (X5 - $0),
                (Y1 - $0) * 10000 +
                  (Y2 - $0) * 1000 +
                  (Y3 - $0) * 100 +
                  (Y4 - $0) * 10 +
                  (Y5 - $0)}|Acc]).

solve_p2(Parsed) ->
  {List1, List2} = lists:unzip(Parsed),
  %% Make a frequency table of the numbers in the right column
  FreqTable = lists:foldl(
                fun(N, Acc) ->
                    maps:update_with(N, fun(Old) -> Old + 1 end, 1, Acc)
                end, #{}, List2),
  lists:foldl(fun(N, Acc) ->
                  N * maps:get(N, FreqTable, 0) + Acc
              end, 0, List1).
