-module(bench).

-export([bench/2]).

bench(Fun, N) ->
  TotalUsecs =
    lists:foldl(
      fun(_, Acc) ->
          {T, _} = timer:tc(Fun),
          T + Acc
      end, 0, lists:seq(1, N)),
  UsecsPerIter = TotalUsecs / N,
  io:format("Executed ~p iterations in ~p usecs/iteration.", [N, UsecsPerIter]).
