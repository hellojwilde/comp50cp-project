-module(registrar).
-export([init/0, init/1, stop/1, state/1, state/2, register_booth/2, register/2,
         register/3]).

% exported functions

init() -> spawn(fun() -> loop([], [], now()) end).

init(Node) -> spawn(Node, fun() -> loop([], [], now()) end).

% start registrar_c.erl

stop(Registrar) ->
    Registrar ! stop,
    ok.

state(Registrar) -> state(Registrar, infinity).

state(Registrar, Timeout) ->
    Registrar ! {state, self()},
    receive
        {state, State} -> State
    after
        Timeout -> {error, "Timed out waiting for registrar state."}
    end.

register_booth(Registrar, Booth) ->
    Registrar ! {register_booth, Booth},
    ok.

% start registrar_vc.erl

register(Registrar, Credentials) -> register(Registrar, Credentials, infinity).

register(Registrar, Credentials, Timeout) ->
    Registrar ! {register, self(), Credentials},
    receive
        {success, _Message} -> _Message;
        {failure, _Message} -> {failure, _Message}
    after
        Timeout -> {error, "Timed out waiting for registration response."}
    end.

% end registrar_vc.erl

% end registrar_c.erl

% other functions

flush() ->
    receive
        _ -> flush()
    after
        0 -> ok
    end.

get_voting_credentials(_Reg_cred) -> _Reg_cred.

% 0 registered booths

loop([], Registered, _Gen_state) ->
    receive
        stop -> flush();
        {state, From} ->
            From ! {state, {[], Registered, _Gen_state}},
            loop([], Registered, _Gen_state);
        {register_booth, Booth} -> loop([Booth], Registered, _Gen_state);
        {register, From, _} ->
            From ! {failure, no_booths},
            loop([], Registered, _Gen_state)
    end;

% 1 registered booth

loop([Booth1], Registered, _Gen_state) ->
    receive
        stop -> flush();
        {state, From} ->
            From ! {state, {[Booth1], Registered, _Gen_state}},
            loop([Booth1], Registered, _Gen_state);
        {register_booth, Booth1} -> loop([Booth1], Registered, _Gen_state);
        {register_booth, Booth} ->
            loop([Booth, Booth1], Registered, _Gen_state);
        {register, From, Reg_cred} ->
            case lists:keymember(Reg_cred, 1, Registered) of
                true ->
                    From ! {failure, registered},
                    loop([Booth1], Registered, _Gen_state);
                false ->
                    Vote_cred = get_voting_credentials(Reg_cred),
                    Booth1 ! {registration, Vote_cred},
                    receive
                        {ok, Vote_cred, Booth1} ->
                            From ! {success, {Vote_cred, Booth1}}
                    end,
                    loop([Booth1], [{Reg_cred, Vote_cred, Booth1} | Registered],
                         _Gen_state)
            end
    end;

% multiple registered booths

loop(Booths, Registered, Gen_state) ->
    receive
        stop -> flush();
        {state, From} ->
            From ! {state, {Booths, Registered, Gen_state}},
            loop(Booths, Registered, Gen_state);
        {register_booth, Booth} ->
            case lists:member(Booth, Booths) of
                true -> loop(Booths, Registered, Gen_state);
                false -> loop([Booth | Booths], Registered, Gen_state)
            end;
        {register, From, Reg_cred} ->
            case lists:keymember(Reg_cred, 1, Registered) of
                true ->
                    From ! {failure, registered},
                    loop(Booths, Registered, Gen_state);
                false ->
                    Vote_cred = get_voting_credentials(Reg_cred),
                    {N, New_gen_state} =
                        random:uniform_s(length(Booths), Gen_state),
                    Booth = lists:nth(N, Booths),
                    Booth ! {registration, Vote_cred},
                    receive
                        {ok, Vote_cred, Booth} ->
                            From ! {success, {Vote_cred, Booth}}
                    end,
                    loop(Booths, [{Reg_cred, Vote_cred, Booth} | Registered],
                         New_gen_state)
            end
    end.