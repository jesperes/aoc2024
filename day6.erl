-module(day6).

-export([solve/1,
         run_n_steps/1]).

-include_lib("eunit/include/eunit.hrl").

-record(labmap, {obstacles :: #{integer() => integer()},
                 coord :: {integer(), integer()}, %% coordinates of the guard
                 dir :: integer(),                %% direction of the guard
                 width :: integer(),
                 height :: integer()}).

solve(Filename) ->
  {ok, Bin} = file:read_file(Filename),
  solve0(Bin, 10).

solve0(Bin, Limit) ->
  Labmap = parse_labmap(Bin),
  walk(Labmap, sets:new(), Limit).

idx_to_coord({Idx, _}, W) ->
  {Idx rem (W + 1), Idx div (W + 1)}.

parse_labmap(Bin) ->
  Lines = [L|_] = binary:split(Bin, <<"\n">>, [global]),
  W = byte_size(L),
  H = length(Lines) - 1,
  Obstacles =
    lists:foldl(
      fun(Idx, Acc) ->
          maps:put(idx_to_coord(Idx, W), $#, Acc)
      end, #{}, binary:matches(Bin, <<"#">>)),
  [Idx] = binary:matches(Bin, <<"^">>),
  #labmap{obstacles = Obstacles,
          coord = idx_to_coord(Idx, W),
          dir = 0,
          width = W,
          height = H}.

walk(Labmap, Visited, 0) ->
  {Labmap, Visited};
walk(#labmap{coord = Coord} = Labmap, Visited, Limit) ->
  %% ?debugFmt("Coord=~p, Visited=~p", [Coord, sets:to_list(Visited)]),
  Visited0 = sets:add_element(Coord, Visited),
  {NextCoord, MaybeNewDir} = next_coord(Labmap),
  walk(Labmap#labmap{coord = NextCoord,
                     dir = MaybeNewDir}, Visited0, Limit - 1).

fwd({X, Y}, Dir) ->
  case Dir of
    0 -> {X, Y - 1};
    1 -> {X + 1, Y};
    2 -> {X, Y + 1};
    3 -> {X - 1, Y}
  end.

next_coord(#labmap{coord = Coord, dir = Dir, obstacles = Obst}) ->
  Fwd = fwd(Coord, Dir),
  case maps:get(Fwd, Obst, undefined) of
    undefined -> {Fwd, Dir}; %% not an obstacle, continue forward
    _Obstacle ->
      NewDir = (Dir + 1) rem 4,  %% obstacle, turn right
      NewCoord = fwd(Coord, NewDir),
      {NewCoord, NewDir}
  end.

to_string(#labmap{width = W, height = H} = Labmap, Visited) ->
  lists:map(
    fun(Y) ->
        lists:map(
          fun(X) ->
              char_at({X, Y}, Labmap, Visited)
          end, lists:seq(0, W - 1)) ++ "\n"
    end, lists:seq(0, H - 1)).

char_at(Coord, #labmap{coord = Player,
                       dir = Dir,
                       obstacles = Obst},
       Visited) ->
  case maps:get(Coord, Obst, undefined) of
    $# -> $#;
    undefined when Coord =:= Player ->
      case Dir of
        0 -> $^;
        1 -> $>;
        2 -> $v;
        3 -> $<
      end;
    _ ->
      case sets:is_element(Coord, Visited) of
        true -> $O;
        false -> $.
      end
  end.

-ifdef(TEST).
ex1() ->
  <<"....#.....\n"
    ".........#\n"
    "..........\n"
    "..#.......\n"
    ".......#..\n"
    "..........\n"
    ".#..^.....\n"
    "........#.\n"
    "#.........\n"
    "......#...\n">>.

run_n_steps(Limit) ->
  Bin = ex1(),
  Labmap = parse_labmap(Bin),
  {Labmap0, Visited} = walk(Labmap, sets:new(), Limit),
  io:format(standard_error, "~s~n", [to_string(Labmap0, Visited)]).

ex1_test() ->
  Bin = ex1(),
  Labmap = parse_labmap(Bin),
  {Labmap0, Visited} = walk(Labmap, sets:new(), 8),
  io:format(standard_error, "~s~n", [to_string(Labmap0, Visited)]).

-endif.
