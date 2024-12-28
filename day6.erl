-module(day6).

-export([solve/1]).

-include_lib("eunit/include/eunit.hrl").

-record(labmap, {obstacles :: #{integer() => integer()},
                 coord :: {integer(), integer()}, %% coordinates of the guard
                 dir :: integer(),                %% direction of the guard
                 width :: integer(),
                 height :: integer()
                }).

%% 1680 too high
%% 1650 not correct
%% 1506 not correct
%% 1505 too low

solve(Filename) ->
  {ok, Bin} = file:read_file(Filename),
  solve0(Bin).

solve0(Bin) ->
  Labmap = parse_labmap(Bin),
  {_Reason, _Labmap0, Visited} =
    walk(Labmap, sets:new()),
  VisitedCoords = visited_coords(Visited),
  Part1 = sets:size(VisitedCoords),
  %% ?debugFmt("Number of visited coordinates: ~p", [Part1]),
  %% Coords = [{X, Y} ||
  %%            Y <- lists:seq(0, Labmap#labmap.height - 1),
  %%            X <- lists:seq(0, Labmap#labmap.width - 1)],
  Coords = lists:sort(sets:to_list(VisitedCoords)),
  Part2 = solve_part2(Coords, Labmap, 0),
  ?debugFmt("Checked ~p coords", [length(Coords)]),
  {Part1, Part2}.

solve_part2([], _, NumLoopCoords) ->
  NumLoopCoords;
solve_part2([Coord|Rest], #labmap{obstacles = Obst} = Labmap, NumLoopCoords) ->
  %% ?debugFmt("Checking if putting an obstacle at ~p causes a loop", [Coord]),
  case walk(Labmap#labmap{obstacles = maps:put(Coord, $#, Obst)}, sets:new()) of
    {cycle_detected, _, _} ->
      ?debugFmt("Loop detected when putting obstacle at ~p", [Coord]),
      solve_part2(Rest, Labmap, NumLoopCoords + 1);
    _  ->
      solve_part2(Rest, Labmap, NumLoopCoords)
  end.

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

%% dir_to_str(0) -> north;
%% dir_to_str(1) -> east;
%% dir_to_str(2) -> south;
%% dir_to_str(3) -> west.

walk(#labmap{coord = Coord,
             dir = Dir,
             obstacles = _Obstacles} = Labmap,
     Visited) ->
  IsVisited = sets:is_element({Coord, Dir}, Visited),
  IsOffMap = is_off_map(Labmap),
  if IsOffMap ->  {off_map, Labmap, Visited};
     IsVisited -> {cycle_detected, Labmap, Visited};
     true ->
      Visited0 = sets:add_element({Coord, Dir}, Visited),
      {NextCoord, MaybeNewDir} = next_coord(Labmap),
      walk(Labmap#labmap{coord = NextCoord,
                         dir = MaybeNewDir},
           Visited0)
  end.


is_off_map(#labmap{coord = {X, Y}, width = W, height = H}) ->
  X < 0 orelse
    X >= W orelse
    Y < 0 orelse
    Y >= H.

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
      %%?debugFmt("Turning at ~p from ~s to ~s", [Coord, dir_to_str(Dir), dir_to_str(NewDir)]),
      {NewCoord, NewDir}
  end.

visited_coords(Visited) ->
  sets:from_list(
    lists:map(fun({Coord, _Dir}) -> Coord end, sets:to_list(Visited))).

%% to_string(#labmap{width = W, height = H} = Labmap, Visited) ->
%%   VisitedCoords = visited_coords(Visited),
%%   lists:map(
%%     fun(Y) ->
%%         lists:map(
%%           fun(X) ->
%%               char_at({X, Y}, Labmap, VisitedCoords)
%%           end, lists:seq(0, W - 1)) ++ "\n"
%%     end, lists:seq(0, H - 1)).

%% char_at(Coord, #labmap{coord = Player,
%%                        dir = Dir,
%%                        obstacles = Obst},
%%        Visited) ->
%%   case maps:get(Coord, Obst, undefined) of
%%     $# -> $#;
%%     undefined when Coord =:= Player ->
%%       case Dir of
%%         0 -> $^;
%%         1 -> $>;
%%         2 -> $v;
%%         3 -> $<
%%       end;
%%     _ ->
%%       case sets:is_element(Coord, Visited) of
%%         true -> $X;
%%         false -> $.
%%       end
%%   end.

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

ex1_test() ->
  Bin = ex1(),
  Labmap = parse_labmap(Bin),
  {_Reason, _Labmap0, Visited} =
    walk(Labmap, sets:new()),
  VisitedCoords = visited_coords(Visited),
  Part1 = sets:size(VisitedCoords),
  Part2 = solve_part2(sets:to_list(VisitedCoords), Labmap, 0),
  ?assertEqual(41, Part1),
  ?assertEqual(6, Part2).

-endif.
