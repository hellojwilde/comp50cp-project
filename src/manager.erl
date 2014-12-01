-module(manager).
-export([init/0]).

% TODO:
% everything related to webserver
%   announcing it to relevant parties
%   maintaining its state in the loop?
%   linking to it?
%   quitting it appropriately (and on crash)
% Getting/making a list of talliers

init() ->
    Registrar = registrar:init(),
    link(Registrar),
    Booths = lists:map(
            fun(_) -> booth:init() end, 
            lists:seq(1, 10)),
    link_to_pid_list(Booths),
    Talliers = [],  % @ANSCHEL, this needs to be a pid list
    link_to_pid_list(Talliers),
    WinnerCollector = winner_collector:init(),
    FrontendData = #frontend_pids{registrar_pid = Registrar, tally_collector_pid = WinnerCollector},
    frontend_sup:start_link(FrontendData),
    lists:map(fun(Booth) -> registrar:register_booth(Registrar, Booth) end, Booths),
    lists:map(
        fun({Booth, Tallier}) -> Tallier ! {announce, Booth} end,
        [{X, Y} || X <- Booths, Y <- Talliers]),
    spawn(fun() -> loop(Registrar, Booths, Talliers) end).

loop(Registrar, Booths, Talliers) ->
    receive
        {newBooth, Booth} ->
            registrar:register_booth(Registrar, Booth),
            lists:map(fun(Tallier) -> Tallier ! {announce, Booth} end, Talliers),
            loop(Registrar, Booths, Talliers, Serverdata);
        {newTallier, Tallier} ->
            lists:map(fun(Booth) -> Tallier ! {announce, Booth} end, Booths),
            % @JONATHAN tell webserver about it?
            loop(Registrar, Booths, Talliers, Serverdata);
        flush ->
            lists:map(fun(Booth) -> Booth ! flush end, Booths),
            loop(Registrar, Booths, Talliers, Serverdata);
        quit ->
            % @JONATHAN do whatever webserver needs here
            ok
    end.

link_to_pid_list(Xs) ->
    lists:map(fun(X) -> link(X) end, Xs).
