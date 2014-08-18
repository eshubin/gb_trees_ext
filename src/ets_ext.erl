-module(ets_ext).

-export([lower_bound/2, select_by_key/2]).

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("eunit/include/eunit.hrl").

lower_bound(Tab, Val) ->
	MatchSpec = ets:fun2ms(
		fun({Key, _}) when Key >= Val->
			Key
		end
		),
	select(Tab, MatchSpec).

select_by_key(Tab, Val) ->
	MatchSpec = [{{Val,'_'},[],[Val]}],
	select(Tab, MatchSpec).

select(Tab, MatchSpec) ->
	case ets:select(Tab, MatchSpec, 1) of
		'$end_of_table' -> [];
		{[_] = E, _} -> E
	end.


lower_bound_test_() ->
  {
    setup,
    fun() ->
    	T = ets:new(lower_bound_test, [ordered_set]),
      	true = ets:insert(T, [{X, X} || X <- lists:seq(0, 20, 2)]),
      	T
    end,
    fun(T) ->
    	ets:delete(T)
    end,
    fun(T) ->
		[
			?_test(
				begin
					Tab = ets:new(lower_bound_test2, [ordered_set]),
					?assertEqual([], lower_bound(Tab, 4)),
					ets:delete(Tab)
				end
			),
			?_assertEqual([], lower_bound(T, 30)),
			?_assertEqual([2], lower_bound(T, 1)),
			?_assertEqual([2], lower_bound(T, 2)),
			?_assertEqual(4, ets:next(T, 2))
		]
    end
  }.

select_test_() ->
  {
    setup,
    fun() ->
    	T = ets:new(lower_bound_test, [ordered_set]),
      	true = ets:insert(T, [{X, X} || X <- lists:seq(0, 20, 2)]),
      	T
    end,
    fun(T) ->
    	ets:delete(T)
    end,
    fun(T) ->
		[
			?_test(
				begin
					Tab = ets:new(lower_bound_test2, [ordered_set]),
					?assertEqual([], select_by_key(Tab, 4)),
					ets:delete(Tab)
				end
			),
			?_assertEqual([], select_by_key(T, 30)),
			?_assertEqual([2], select_by_key(T, 2)),
			?_assertEqual(4, ets:next(T, 2))
		]
    end
  }.



performance_test_() ->
  {
    setup,
    fun() ->
    	T = ets:new(lower_bound_test, [ordered_set]),
      	true = ets:insert(T, [{X, X} || X <- lists:seq(0, 20000000, 1)]),
      	T
    end,
    fun(T) ->
    	ets:delete(T)
    end,
    fun(T) ->
		[
			test_lower_bound(T),
			test_select_by_key(T)
		]
	end
  }.

-define(SEARCH_KEYS, [10000, 100000, 1000000, 10000000, 20000000]).

test_lower_bound(T) ->
	test_perf(T, lower_bound).

test_select_by_key(T) ->
	test_perf(T, select_by_key).

test_perf(T, F) ->
	fun() ->
		lists:foreach(
			fun(K) ->
				?assertEqual([K], ?debugTime(integer_to_list(K), ets_ext:F(T, K)))
			end,
			?SEARCH_KEYS
		),
		?debugMsg("======================")
	end.

