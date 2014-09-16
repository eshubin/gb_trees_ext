gb_trees_ext
============

Extension of gb_trees module of Erlang OTP with lower bound algorithm.
src/ets_ext.erl - shows that ets:select function does full table scan,
if range of keys is requested, even with ETS' type of ordered_set.
