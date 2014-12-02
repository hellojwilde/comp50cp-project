-module(booth).
-export([init/0]).

batchfreq() -> 5.

init() ->
    spawn(fun() -> loop(sets:new(), [], [], [], batchfreq()) end).

loop(Not_yet_voted, Unbatched_ballots, Batched_ballots, Voting_schemes, BatchFreq) ->
    receive
        % register a new voter at this location
        {registration, Pid, Credentials} ->
            New_not_yet_voted = sets:add_element(Credentials, Not_yet_voted),
            Pid ! {registered, Credentials, self()},
            loop(New_not_yet_voted, Unbatched_ballots, Batched_ballots, Voting_schemes, BatchFreq);
        % cast a vote
        {ballot, Pid, Credentials, Data} ->
            Valid_registration = sets:is_element(Credentials, Not_yet_voted),
            if
                Valid_registration ->
                    Pid ! success,
                    New_not_yet_voted = sets:del_element(Credentials, Not_yet_voted),
                    New_unbatched_ballots = [Data | Unbatched_ballots],
                    if
                        length(New_unbatched_ballots) >= BatchFreq ->
                            Fun = fun({TallierPid,Batchfun}) -> TallierPid ! {batch, Batchfun(New_unbatched_ballots)} end,
                            lists:map(Fun, Voting_schemes),
                            loop(New_not_yet_voted, [], Batched_ballots ++ New_unbatched_ballots, Voting_schemes, BatchFreq);
                        true -> % not time to batch
                            loop(New_not_yet_voted, New_unbatched_ballots, Batched_ballots, Voting_schemes, BatchFreq)
                    end;
                true -> % invalid registration
                    Pid ! {failure, invalid_registration},
                    loop(Not_yet_voted, Unbatched_ballots, Batched_ballots, Voting_schemes, BatchFreq)
            end;
        % add a new ballot tallying system
        {newScheme, Batchfun, Pid} ->
            New_schemes = [{Pid, Batchfun} | Voting_schemes],
            % resend all unbatched ballots
            Pid ! {batch, Batchfun(Batched_ballots)},
            loop(Not_yet_voted, Unbatched_ballots, Batched_ballots, New_schemes, BatchFreq);
        % send all unbatched data
        flush ->
            Fun = fun({Pid,Batchfun}) -> Pid ! {batch, Batchfun(Unbatched_ballots)} end,
            lists:map(Fun, Voting_schemes),
            loop(Not_yet_voted, [], Batched_ballots ++ Unbatched_ballots, Voting_schemes, BatchFreq);
        quit ->
            ok
    end.
