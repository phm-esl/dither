-module(dither).

-export([sample/1]).

% https://en.wikipedia.org/wiki/Dither

-spec sample(function() | nonempty_list({pos_integer(),any()}))
   -> {any(),function()}.
sample(Fn) when is_function(Fn) ->
  % function closure contains all needed context: just call it.
  Fn();
sample([{_,Pick}]) ->
  % Pick is the only option
  {Pick, fun X () -> {Pick, X} end};
sample(Set) when [] /= Set ->
  % First invocation switches from Set list to tuple {Set,Max}
  Dither = fun X (In) ->
    {Pick,Out} = dither(In),
    {Pick,fun () -> X(Out) end} end,
  Dither(prepare(Set)).

-spec prepare(nonempty_list({pos_integer(),any()}))
   -> { nonempty_list(
          { integer(),pos_integer(),any() } ),
        pos_integer() }.
prepare(In) -> prepare(In,0,[]).

prepare([],Max,Out) ->
  ready(Max,Out,[]);
prepare([{Ratio,_} = In|Rest], Max, Out) ->
  % Max is sum of all ratios, represents 100%
  prepare(Rest, Max + Ratio, [In|Out]).

ready(Max,[],Out) ->
  {Out, Max};
ready(Max,[{Ratio,Pick}|Rest], Out) ->
  % Make all available Pick equally likely to be picked at first
  ready(Max,Rest,[{Max - Ratio,Ratio,Pick}|Out]).

bump({Total,Ratio,Pick}) ->
  {Total + Ratio, Ratio, Pick}.

-spec dither({nonempty_list({integer(),pos_integer(),any()}),pos_integer()})
   -> { any(),
        { nonempty_list({integer(),pos_integer(),any()}),
          pos_integer() } }.
dither({Set,Max}) ->
  Bump = lists:map(fun bump/1, Set),
  Part = fun({Total,_,_}) -> Max < Total end,
  case lists:partition(Part, Bump) of
    {[],Low} ->
      [{Total,Ratio,Pick}|Rest] = Low,
      {Pick, {Rest ++ [{Total - Max, Ratio, Pick}], Max} };
    {High,Low} ->
      [{Total,Ratio,Pick}|Rest] = High,
      {Pick, {Rest ++ Low ++ [{Total - Max, Ratio, Pick}], Max} } end.
