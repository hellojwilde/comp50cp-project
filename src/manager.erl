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

    % frontend
    FrontendData = #frontend_pids{
        registrar_pid = Registrar, 
        tally_collector_pid = WinnerCollector
    },
    frontend_sup:start_link(FrontendData),

    lists:map(fun(Booth) -> registrar:register_booth(Registrar, Booth) end, Booths),
    lists:map(
        fun({Booth, Tallier}) -> Tallier ! {announce, Booth} end,
        [{X, Y} || X <- Booths, Y <- Talliers]),
    spawn(fun() -> loop(Registrar, Booths, Talliers, WinnerCollector) end).

loop(Registrar, Booths, Talliers, WinnerCollector) ->
    receive
        {newBooth, Booth} ->
            registrar:register_booth(Registrar, Booth),
            lists:map(fun(Tallier) -> Tallier ! {announce, Booth} end, Talliers),
            loop(Registrar, Booths, Talliers, WinnerCollector);
        {newPointScheme, PointScheme, PointSchemeName} ->
            Tallier = tally:init(
                PointScheme, 
                winner_collector:get_winner_collection_fn(
                    PointSchemeName, 
                    WinnerCollector
                )
            ),
            lists:map(fun(Booth) -> Tallier ! {announce, Booth} end, Booths),
            loop(Registrar, Booths, Talliers, WinnerCollector);
        flush ->
            lists:map(fun(Booth) -> Booth ! flush end, Booths),
            loop(Registrar, Booths, Talliers, WinnerCollector);
        quit ->
            ok
    end.

link_to_pid_list(Xs) ->
    lists:map(fun(X) -> link(X) end, Xs).

schemes() ->
    [
        {"Plurality", point_scheme:plurality()}, 
        {"Approval", point_scheme:approval()}, 
        {"Borda Count", point_scheme:borda_count()}, 
        {"Nauru", point_scheme:nauru()}, 
        {"Schultze", schultze}
    ].

