-module(booth).
-export([loop/4, init/0]).

init() ->
    spawn(fun() -> loop(sets:new(), [], [], maps:new()) end).

loop(Not_yet_voted, Unbatched_ballots, Batched_ballots, Voting_schemes) ->
    receive
        {registration, Credentials} ->
            New_not_yet_voted = sets:add_element(Credentials, Not_yet_voted),
            loop(New_not_yet_voted, Unbatched_ballots, Batched_ballots, Voting_schemes);
        {ballot, Credentials, Data} ->
            Valid_registration = sets:is_element(Credentials, Not_yet_voted),
            if
                Valid_registration ->
                    %New_not_yet_voted = sets:remove(Credentials, Not_yet_voted),
                    New_unbatched_ballots = [Data | Unbatched_ballots],
                    %loop(New_not_yet_voted, New_unbatched_ballots, Batched_ballots, Voting_schemes)
                    loop(Not_yet_voted, New_unbatched_ballots, Batched_ballots, Voting_schemes);
                true ->
                    bad_ballot
            end;
        % new scheme addition
        {newScheme, Batchfun, Pid} ->
            New_schemes = maps:put(Pid, Batchfun, Voting_schemes),
            % resend all unbatched ballots
            Pid ! {batch, Batchfun(Batched_ballots)},
            loop(Not_yet_voted, Unbatched_ballots, Batched_ballots, New_schemes);
        flush ->
            Fun = fun(Pid,Batchfun) -> Pid ! {batch, Batchfun(Unbatched_ballots)} end,
            maps:map(Fun, Voting_schemes),
            loop(Not_yet_voted, [], Batched_ballots ++ Unbatched_ballots, Voting_schemes);
        quit ->
            ok
    end.
