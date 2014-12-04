-module(all_votes).
-export([batch/1, empty/0, combine/2, winner/1]).

empty() -> [].
combine(A, B) -> A ++ B.
winner(A) -> A.
batch(Ballots) -> lists:map(fun({vote, [Top | _Rest], _App}) -> Top end, Ballots).