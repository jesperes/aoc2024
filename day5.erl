-module(day5).

-export([solve/1]).

-include_lib("eunit/include/eunit.hrl").

solve(Filename) ->
  {ok, Bin} = file:read_file(Filename),
  solve0(Bin).

solve0(Bin) ->
  [Section1, Section2] = binary:split(Bin, <<"\n\n">>),
  Rules =
    lists:foldl(
      fun(<<A, B, $|, C, D>>, Acc) ->
          [{binary_to_integer(<<A, B>>), binary_to_integer(<<C, D>>)}|Acc]
      end, [], binary:split(Section1, <<"\n">>, [global])),

  Updates =
    lists:map(fun(Update) ->
                  lists:map(fun(B) -> binary_to_integer(B) end, binary:split(Update, <<",">>, [global]))
              end, binary:split(string:trim(Section2), <<"\n">>, [global])),

  {solve_p1(Updates, Rules),
   solve_p2(Updates, Rules)}.

 solve_p1(Updates, Rules) ->
  lists:foldl(
    fun([<<>>], Acc) -> Acc;
       (Update, Acc) ->
        case are_all_correctly_ordered(Update, Rules) of
          true -> get_middle(Update);
          false -> 0
        end + Acc
    end, 0, Updates).

are_all_correctly_ordered([_], _) ->
  true;
are_all_correctly_ordered([A|Rest], Rules) ->
  case is_one_correctly_ordered(A, Rest, Rules) of
    true -> are_all_correctly_ordered(Rest, Rules);
    false -> false
  end.

is_one_correctly_ordered(_, [], _) -> true;
is_one_correctly_ordered(A, [B|Rest], Rules) ->
  %% Check that there is no rule which states that B must come before A.
  (not lists:member({B, A}, Rules)) andalso
    is_one_correctly_ordered(A, Rest, Rules).

get_middle(List) ->
  Len = length(List),
  1 = Len rem 2, %% List must have an odd number of elements
  {_, [Middle|_]} = lists:split(Len div 2, List),
  Middle.

%% p2 is almost same as p1, but we need to use the incorrect elements
%% instead, sorting them first.
solve_p2(Updates, Rules) ->
  lists:foldl(
    fun([<<>>], Acc) -> Acc;
       (Update, Acc) ->
        case are_all_correctly_ordered(Update, Rules) of
          true -> 0;
          false -> get_middle(sort_update(Update, Rules))
        end + Acc
    end, 0, Updates).

sort_update(Update, Rules) ->
  lists:sort(
    fun(A, B) ->
        lists:member({A, B}, Rules)
    end, Update).

-ifdef(TEST).
test_binary() ->
  <<"47|53\n"
    "97|13\n"
    "97|61\n"
    "97|47\n"
    "75|29\n"
    "61|13\n"
    "75|53\n"
    "29|13\n"
    "97|29\n"
    "53|29\n"
    "61|53\n"
    "97|53\n"
    "61|29\n"
    "47|13\n"
    "75|47\n"
    "97|75\n"
    "47|61\n"
    "75|61\n"
    "47|29\n"
    "75|13\n"
    "53|13\n"
    "\n"
    "75,47,61,53,29\n"
    "97,61,53,29,13\n"
    "75,29,13\n"
    "75,97,47,61,53\n"
    "61,13,29\n"
    "97,13,75,29,47\n">>.

example_test() ->
  ?assertEqual({143, 0}, solve0(test_binary())).

-endif.
