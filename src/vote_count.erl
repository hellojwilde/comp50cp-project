-module(vote_count).
-export([batch/1, empty/0, combine/2, winner/1]).

empty() -> 0.

combine(A, B) -> A + B.

winner(A) -> [A].

batch(L) -> length(L).