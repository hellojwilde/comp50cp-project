-module(manager).
-export([init/0]).
-include("../include/vote.hrl").

init() ->
    % registrar
    Registrar = registrar:init(),
    link(Registrar),

    % booths
    Booths = lists:map(
            fun(_) -> booth:init() end, 
            lists:seq(1, 10)),
    link_to_pid_list(Booths),

    % talliers
    WinnerCollector = winner_collector:init(),
    Talliers = lists:map(
            fun({Scheme_atom, Scheme}) -> 
                Update = winner_collector:get_winner_collection_fn(
                    Scheme_atom, 
                    WinnerCollector
                ),
                tally:init(Scheme, Update)
            end,
            schemes()
    ),
    link_to_pid_list(Talliers),

    % tell registrar about all booths
    lists:foreach(
        fun(Booth) -> registrar:register_booth(Registrar, Booth) end,
        Booths),
    % Tells all booths about all talliers
    lists:foreach(
        fun({Booth, Tallier}) -> Tallier ! {announce, Booth} end,
        [{X, Y} || X <- Booths, Y <- Talliers]),

    Manager = spawn(fun() -> loop(Registrar, Booths, Talliers, WinnerCollector) end),

    % frontend
    FrontendData = #frontend_config{
        registrar_pid = Registrar, 
        winner_collector_pid = WinnerCollector,
        manager_pid = Manager,
        candidates = candidates()
    },
    frontend_sup:start_link(FrontendData),

    Manager.    

loop(Registrar, Booths, Talliers, WinnerCollector) ->
    receive
        {newBooth, Booth} ->
            registrar:register_booth(Registrar, Booth),
            lists:foreach(
                fun(Tallier) -> Tallier ! {announce, Booth} end, 
                Talliers),
            loop(Registrar, Booths, Talliers, WinnerCollector);
        {newPointScheme, PointScheme, PointSchemeName} ->
            Tallier = tally:init(
                PointScheme, 
                winner_collector:get_winner_collection_fn(
                    PointSchemeName, 
                    WinnerCollector
                )
            ),
            lists:foreach(
                fun(Booth) -> Tallier ! {announce, Booth} end,
                Booths),
            loop(Registrar, Booths, [Tallier | Talliers], WinnerCollector);
        flush ->
            lists:foreach(fun(Booth) -> Booth ! flush end, Booths),
            loop(Registrar, Booths, Talliers, WinnerCollector);
        quit ->
            registrar:stop(Registrar),
            lists:foreach(fun(Booth) -> Booth ! quit end, Booths),
            lists:foreach(fun(Tallier) -> Tallier ! stop end, Talliers),
            ok
    end.

link_to_pid_list(Xs) ->
    lists:foreach(fun(X) -> link(X) end, Xs).

schemes() ->
    [
%        {"Vote Count", vote_count},
%        {"All Votes", all_votes},
        {"Plurality", point_scheme:plurality()}, 
        {"Approval", point_scheme:approval()}, 
        {"Borda Count", point_scheme:borda_count()}, 
        {"Nauru", point_scheme:nauru()}, 
        {"Schultze", schultze}
    ].

candidates() ->
    [
        "Anschel",
        "Josh",
        "Andrew",
        "Jon"
    ].

