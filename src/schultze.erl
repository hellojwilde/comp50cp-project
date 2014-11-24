-module(schultze).
-export([batch/1, empty/0, combine/2, winner/1]).

% Floyd-Warshall algorithm
% The idea is to progressively allow paths to use one additional node
% We assume the Graph is normalized and has zeroes added
widestPaths(Candidates, Graph) ->
	lists:foldl(fun(Pivot, Paths) ->
		maps:map(fun({First, Second}, OldWidth) ->
			if
				(First == Pivot) or (Second == Pivot) ->
					OldWidth;
				true ->
					ToPivot = maps:get({First, Pivot}, Paths),
					FromPivot = maps:get({Pivot, Second}, Paths),
					ThroughPivot = min(ToPivot, FromPivot),
					max(OldWidth, ThroughPivot)
			end
		end, Paths)
	end, Graph, Candidates).

% Assumes the Graph is normalized
winner(Graph) ->
	Candidates = pairwise:candidates(Graph),
	Zeroed = pairwise:addZeroes(Candidates, Graph),
	Paths = widestPaths(Candidates, Zeroed),
	lists:filter(fun(Candidate) ->
		lists:all(fun(Other) ->
			if
				Other == Candidate ->
					true;
				true ->
					Win = maps:get({Candidate, Other}, Paths),
					Lose = maps:get({Other, Candidate}, Paths),
					Win >= Lose
			end
		end, Candidates)
	end, Candidates).

empty() -> count_map:new().

batch(Ballots) -> pairwise:normalize(pairwise:fromBallots(Ballots)).

combine(G1, G2) -> count_map:sum(G1, G2).
