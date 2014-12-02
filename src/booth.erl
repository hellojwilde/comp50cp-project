-module(booth).
-export([init/0]).

batchfreq() -> 5.

init() ->
    spawn(fun() -> loop(sets:new(), [], [], [], batchfreq()) end).

loop(NotYetVoted, Unsent, Sent, Talliers, BatchFreq) ->
    receive
        % register a new voter at this location
        {registration, Pid, Credentials} ->
            NewNotYetVoted = sets:add_element(Credentials, NotYetVoted),
            Pid ! {registered, Credentials, self()},
            loop(NewNotYetVoted, Unsent, Sent, Talliers, BatchFreq);
        % cast a vote
        {ballot, Pid, Credentials, Data} ->
            ValidRegistration = sets:is_element(Credentials, NotYetVoted),
            if
                ValidRegistration ->
                    Pid ! success,
                    NewNotYetVoted = sets:del_element(Credentials, NotYetVoted),
                    NewUnsent = [Data | Unsent],
                    if
                        length(NewUnsent) >= BatchFreq ->
                            Fun = fun({TallierPid,Batchfun}) -> 
                                TallierPid ! {batch, Batchfun(NewUnsent)} 
                            end,
                            lists:foreach(Fun, Talliers),
                            loop(NewNotYetVoted, [], 
                                Sent ++ NewUnsent, Talliers, BatchFreq);
                        true -> % not time to batch
                            loop(NewNotYetVoted, NewUnsent, 
                                Sent, Talliers, BatchFreq)
                    end;
                true -> % invalid registration
                    Pid ! {failure, invalid_registration},
                    loop(NotYetVoted, Unsent, Sent, Talliers, BatchFreq)
            end;
        % add a new ballot tallying system
        {newScheme, Batchfun, Pid} ->
            NewSchemes = [{Pid, Batchfun} | Talliers],
            % resend all unbatched ballots
            Pid ! {batch, Batchfun(Sent)},
            loop(NotYetVoted, Unsent, Sent, NewSchemes, BatchFreq);
        % send all unbatched data
        flush ->
            Fun = fun({Pid,Batchfun}) -> Pid ! {batch, Batchfun(Unsent)} end,
            lists:foreach(Fun, Talliers),
            loop(NotYetVoted, [], Sent ++ Unsent, Talliers, BatchFreq);
        quit ->
            ok
    end.
