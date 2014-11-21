-module(count_map).
-export([add/3, inc/2, new/0, sum/2, max/1]).

inc(Key, M) -> add(Key, 1, M).

add(Key, ToAdd, M) ->
	Count = maps:get(Key, M, 0),
	maps:put(Key, Count + ToAdd, M).

new() -> maps:new().

sum(M1, M2) ->
	maps:fold(fun add/3, M1, M2).

max(M) ->
	{Keys, _Count} = maps:fold(fun(Key, Count, {Winners, Max}) ->
		if
			Count > Max -> {[Key], Count};
			Count =:= Max -> {[Key | Winners], Max};
			true -> {Winners, Max}
		end
	end, {[], 0}, M),
	Keys.