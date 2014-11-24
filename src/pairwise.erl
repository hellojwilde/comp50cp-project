-module(pairwise).
-export([fromBallots/1, normalize/1, candidates/1, addZeroes/2]).

pairs([]) -> [];
pairs([_]) -> [];
pairs([First | Rest]) ->
	FirstPairs = lists:map(fun(X) -> {First, X} end, Rest),
	RestPairs = pairs(Rest),
	FirstPairs ++ RestPairs.

addPairs(Pairs, Graph) ->
	lists:foldl(fun count_map:inc/2, Graph, Pairs).

fromBallots(Ballots) ->
	Pairs = lists:map(fun({vote, Choice, _Approved}) -> pairs(Choice) end, Ballots),
	lists:foldl(fun addPairs/2, count_map:new(), Pairs).

% normalize/1 subtracts {anschel, jonathan} from {jonathan, anschel} and vice versa
normalize(Graph) ->
	maps:map(fun({First, Second}, WinCount) ->
		LoseCount = maps:get({Second, First}, Graph, 0),
		WinCount - LoseCount
	end, Graph).

candidates(Graph) ->
	sets:to_list(maps:fold(fun ({First, Second}, _Count, Cands) ->
		sets:add_element(First, sets:add_element(Second, Cands))
	end, sets:new(), Graph)).

addZeroes(Candidates, Graph) ->
	lists:foldl(fun({First, Second}, G) ->
		count_map:add({First, Second}, 0,
			count_map:add({Second, First}, 0, G))
	end, Graph, pairs(Candidates)).
