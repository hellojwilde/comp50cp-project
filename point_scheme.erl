-module(point_scheme).
-export([make_scheme/1, plurality/0, approval/0, borda_count/0, nauru/0]).

% For schemes that assign points to each candidate on the ballot, where the winner is the candidate with most points.

empty() -> count_map:new().
combine(B1, B2) -> count_map:sum(B1, B2).
winner(B) -> count_map:max(B).

make_batch(Points_fun) -> fun(Ballots) ->
	Points = lists:map(Points_fun, Ballots),
	lists:foldl(fun count_map:sum/2, empty(), Points)
	end.

% Points_fun is a function that takes a single ballot and returns a count_map:
% each candidate is mapped to the number of points that candidate receives
make_scheme(Points_fun) -> {
	make_batch(Points_fun),
	fun empty/0,
	fun combine/2,
	fun winner/1
	}.

plurality() -> make_scheme(fun({vote, [Favorite | _Rest], _Approved}) ->
	count_map:inc(Favorite, count_map:new())
	end).

approval() -> make_scheme(fun({vote, Candidates, Approved}) ->
	Good_Cands = lists:sublist(Candidates, Approved),
	lists:foldl(fun count_map:inc/2, count_map:new(), Good_Cands)
	end).

borda_count() -> make_scheme(fun({vote, Candidates, _Approved}) ->
	{Counts, _} = lists:foldl(
		fun(Candidate, {Counts, Score}) ->
			{count_map:add(Candidate, Score, Counts), Score - 1}
		end,
		{count_map:new(), length(Candidates)},
		Candidates),
	Counts
	end).

nauru() -> make_scheme(fun({vote, Candidates, _Approved}) ->
	{Counts, _} = lists:foldl(
		fun(Candidate, {Counts, N}) ->
			{count_map:add(Candidate, 1/N, Counts), N + 1}
		end,
		{count_map:new(), 1},
		Candidates),
	Counts
	end).
