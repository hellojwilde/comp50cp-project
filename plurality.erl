-module(plurality).
-export([batch/1, empty/0, combine/2, winner/1]).

empty() -> count_map:new().

combine(B1, B2) -> count_map:sum(B1, B2).

winner(B) -> count_map:max(B).

batch(Votes) ->
	lists:foldl(
		fun({vote, [First | _], _Approve}, B) -> count_map:inc(First, B) end,
		empty(),
		Votes
	).