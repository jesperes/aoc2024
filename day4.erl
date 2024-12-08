-module(day4).

-export([solve/1]).

-include_lib("eunit/include/eunit.hrl").

solve(Filename) ->
  {ok, Bin} = file:read_file(Filename),
  Map = to_map(Bin),
  {find_xmas(Map),
   find_x_mas(Map)}.

to_map(Bin) ->
  Lines = [L|_] = binary:split(Bin, <<"\n">>, [global]),
  Width = byte_size(L),
  Height = length(Lines) - 1, %% exclude last empty line

  %% Convert the input binary to a map of {X, Y} -> Letter
  lists:foldl(fun({X, Y} = Pos, Acc) ->
                  maps:put(Pos, binary:at(Bin, X + Y * (Width + 1)), Acc)
              end, #{},
              [{X, Y} ||
                X <- lists:seq(0, Width - 1),
                Y <- lists:seq(0, Height - 1)]).

find_xmas(Map) ->
  %% Find all the X:s
  Xs = maps:fold(fun(Pos, $X, Acc) -> [Pos|Acc];
                    (_, _, Acc) -> Acc
                 end, [], Map),

  Deltas = [{Dx, Dy} || Dx <- [-1, 0, 1],
                        Dy <- [-1, 0, 1],
                        {Dx, Dy} =/= {0, 0}],

  %% For each X, check each of the 8 directions
  lists:foldl(fun(Pos, Acc) ->
                  num_xmases(Pos, Deltas, Map, 0) + Acc
              end, 0, Xs).

num_xmases(_Pos, [], _Map, Acc) -> Acc;
num_xmases(Pos, [Delta|Deltas], Map, Acc) ->
  case is_xmas(Pos, Delta, Map) of
    true -> num_xmases(Pos, Deltas, Map, Acc + 1);
    false -> num_xmases(Pos, Deltas, Map, Acc)
  end.

is_xmas({X, Y}, {Dx, Dy}, Map) ->
  case lists:map(fun(N) ->
                     maps:get({X + Dx * N, Y + Dy * N}, Map, undef)
                 end, lists:seq(0, 3)) of
    "XMAS" -> true;
    "SAMX" -> true;
    _ -> false
  end.

find_x_mas(Map) ->
  %% Find all the A:s
  As = maps:fold(fun(Pos, $A, Acc) -> [Pos|Acc];
                    (_, _, Acc) -> Acc
                 end, [], Map),

  Deltas = [{-1, -1}, {-1, 1}, {1, -1}, {1, 1}],

  lists:foldl(
    fun(Pos, Acc) ->
        num_x_mases(Pos, Deltas, Map) + Acc
    end, 0, As).

num_x_mases({X, Y}, Deltas, Map) ->
  Diagonals =
    lists:foldl(
      fun({Dx, Dy}, Acc) ->
          case maps:get({X + Dx, Y + Dy}, Map, undefined) of
            undefined -> Acc;
            Key -> [Key|Acc]
          end
      end, [], Deltas),

  case Diagonals of
    "MSMS" -> 1;
    "MMSS" -> 1;
    "SMSM" -> 1;
    "SSMM" -> 1;
    _ -> 0
  end.

-ifdef(TEST).

ex1_test() ->
  Bin = <<"MMMSXXMASM\n"
          "MSAMXMSMSA\n"
          "AMXSXMAAMM\n"
          "MSAMASMSMX\n"
          "XMASAMXAMM\n"
          "XXAMMXXAMA\n"
          "SMSMSASXSS\n"
          "SAXAMASAAA\n"
          "MAMMMXMMMM\n"
          "MXMXAXMASX\n">>,
  Map = to_map(Bin),
  ?assertEqual(18, find_xmas(Map)).

ex2_test() ->
  Bin = <<"MMMSXXMASM\n"
          "MSAMXMSMSA\n"
          "AMXSXMAAMM\n"
          "MSAMASMSMX\n"
          "XMASAMXAMM\n"
          "XXAMMXXAMA\n"
          "SMSMSASXSS\n"
          "SAXAMASAAA\n"
          "MAMMMXMMMM\n"
          "MXMXAXMASX\n">>,
  Map = to_map(Bin),
  ?assertEqual(18, find_x_mas(Map)).

-endif.
