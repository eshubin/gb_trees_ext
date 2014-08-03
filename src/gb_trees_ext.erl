%% Copyright
-module(gb_trees_ext).
-author("eshubin").

%% API
-export([lower_bound/2]).


lower_bound(Key, {_, T}) ->
  lower_bound_1(Key, T, []).


lower_bound_1(Key, {Key1, _, Smaller, _} = T, As) when Key < Key1 ->
  lower_bound_1(Key, Smaller, [T | As]);
lower_bound_1(Key, {Key1, _, _, Bigger}, As) when Key > Key1 ->
  lower_bound_1(Key, Bigger, As);
lower_bound_1(Key, {Key1, _, _, _} = T, As) when Key == Key1 ->
  [T | As];
lower_bound_1(_, nil, As) ->
  As.


-include_lib("eunit/include/eunit.hrl").

lower_bound_test_() ->
  {
    setup,
    fun() ->
      gb_trees:from_orddict([{X, X} || X <- lists:seq(0, 20, 2)])
    end,
    fun(Tree) ->
      [
        ?_assertEqual([], lower_bound(3, gb_trees:empty())),
        ?_assertMatch({4,4,_}, gb_trees:next(lower_bound(3, Tree))),
        ?_assertMatch({4,4,_}, gb_trees:next(lower_bound(4, Tree))),
        ?_assertMatch(none, gb_trees:next(lower_bound(40, Tree))),
        ?_assertMatch({6,6,_}, gb_trees:next(lower_bound(5, Tree))),
        ?_test(
          begin
            {K1, V1, I2} = gb_trees:next(lower_bound(17, Tree)),
            ?assertEqual({18,18}, {K1, V1}),
            {K2, V2, I3} = gb_trees:next(I2),
            ?assertEqual({20,20}, {K2, V2}),
            ?assertEqual(none, gb_trees:next(I3))
          end
        ),
        ?_test(
          begin
            {K1, V1, I2} = gb_trees:next(lower_bound(2, Tree)),
            ?assertEqual({2,2}, {K1, V1}),
            {K2, V2, _I3} = gb_trees:next(I2),
            ?assertEqual({4,4}, {K2, V2})
          end
        )

      ]
    end
  }.