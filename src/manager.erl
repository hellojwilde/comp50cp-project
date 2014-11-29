-module(manager).
-export([init/0]).

% todo, link stuff?
% loop should be able to add booths and talliers
% everything related to webserver
%   announcing it to relevant parties
%   maintaining its state in the loop?
%   quitting it appropriately (and on crash)
%       to discuss, better quit method for everyone

init() ->
    Registrar = registrar:init(),
    Booths = lists:map(
            fun(_) -> booth:init() end, 
            lists:seq(0, 10)),
    Talliers = [],  % get this from Anschel
    Serverdata = [], % ???
    lists:map(fun(Booth) -> registrar:register_booth(Registrar, Booth), Booths),
    map(
        fun({Booth, Tallier} -> Tallier ! {announce, Booth}),
        [{X, Y} || X <- Booths, Y <- Talliers])
    spawn(fun() -> loop(Registrar, Booths, Talliers, Serverdata) end).

loop(Registrar, Booths, Talliers, Serverdata) ->
    io:format("hi~n"),
    receive
        flush ->
            lists:map(fun(Booth) -> Booth ! flush end, Booths),
            loop(Registrar, Booths, Talliers, Serverdata);
        quit ->
            % do whatever webserver needs here
            ok
    end.
