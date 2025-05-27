-module(dither_tests).

-export([test/0]).

test() ->
  % test using prime numbers for population sizes
  Populations = [11,101,1009,10007],
  Suite = #{
    ?LINE => #{
      % number of types greater than population
      assert => [#{
        1 => 1, 2 => 1, 3 => 1, 4 => 1, 5 => 1, 6 => 1,
        7 => 1, 8 => 1, 9 => 1, 10 => 1, 11 => 1}],
      test => set_test([{1,X} || X <- lists:seq(1,101)],[11])},
    ?LINE => #{
      % equal ratios of three types in population
      assert => [
        #{a =>    4,b =>    4,c =>    3},
        #{a =>   34,b =>   34,c =>   33},
        #{a =>  337,b =>  336,c =>  336},
        #{a => 3336,b => 3336,c => 3335} ],
      test => set_test([{1,a},{1,b},{1,c}],Populations) },
    ?LINE => #{
      % no types at all is an error
      error => function_clause,
      test => set_test([],Populations) },
    ?LINE => #{
      % unequal integer ratios of three types in population
      test => set_test([{1,a},{3,b},{6,c}],Populations),
      assert => [
        #{a =>    2,b =>    3,c =>    6},
        #{a =>   11,b =>   30,c =>   60},
        #{a =>  101,b =>  303,c =>  605},
        #{a => 1001,b => 3002,c => 6004} ] },
    ?LINE => #{
      % unequal float ratios of three types in population
      test => set_test([{1/5,a},{3/5,b},{6/5,c}],Populations),
      assert => [
        % smaller population mix different to integer ratios
        #{a =>    1,b =>    4,c =>    6},
        #{a =>   10,b =>   31,c =>   60},
        % bigger population mix same as integer ratios
        #{a =>  101,b =>  303,c =>  605},
        #{a => 1001,b => 3002,c => 6004} ] },
    ?LINE => #{
      % edge case, entire population of unique type
      test => set_test([{99,a}],Populations),
      assert => [
        #{a => 11},
        #{a => 101},
        #{a => 1009},
        #{a => 10007}] } },
  execute_tests(Suite).

set_test(In,Populations) ->
  First = fun () -> dither:sample(In) end,
  Each = fun (Nbr) -> collect(Nbr,First,#{}) end,
  fun () -> lists:map(Each, Populations) end.

collect(0, _, Out) -> Out;
collect(N, Fn, Out) ->
  {Pick,Next} = Fn(),
  case Out of
    #{ Pick := I } -> collect(N - 1, Next, Out#{ Pick => I + 1 });
    #{ } -> collect(N - 1, Next, Out#{ Pick => 1 }) end.

execute_tests(Suite) when is_map(Suite) ->
  each_test(maps:next(maps:iterator(Suite))).

each_test(In) ->
  case In of
    none -> ok;
    {Line_nbr, Data, Iter} ->
      test_input(Line_nbr, Data),
      each_test(maps:next(Iter)) end.

test_input(Line_nbr, Data) ->
  try (maps:get(test,Data))() of
    Out when is_map_key(assert,Data) ->
      Assert = maps:get(assert,Data),
      {Line_nbr,Out,Assert} = {Line_nbr,Assert,Out};
    _ -> true
  catch
    What:Why when is_map_key(What,Data) ->
      Assert = maps:get(What,Data),
      {Line_nbr,Why,Assert} = {Line_nbr,Assert,Why} end.
