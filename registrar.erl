-module(registrar).
-export([init/0, init/1, stop/1, state/1, state/2, register_booth/2, register/2,
         register/3]).

% exported functions

init() -> spawn(fun() -> loop() end).

init(Node) -> spawn(Node, fun() -> loop() end).

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

get_voting_credentials(Args) ->
    {Vote_creds, State} = Args,
    Pass = random_pass(16, [], State),
    case lists:member(Pass, Vote_creds) of
        true -> get_voting_credentials({Vote_creds, now()});
        false -> Pass
    end.

random_pass(0, _Pass, _) -> _Pass;
random_pass(N, Pass, State) ->
    {C, New_state} = random_alpha_num(State),
    random_pass(N - 1, [C | Pass], New_state).

random_alpha_num(State) ->
    {N, New_state} = random:uniform_s(62, State),
    C = if
        N < 11 -> N + 47;
        N < 37 -> N + 54;
        true -> N + 60
    end,
    {C, New_state}.

% 0 registered booths

loop() ->
    receive
        stop -> flush();
        {state, From} ->
            From ! {state, {[], [], [], [], no_gen}},
            loop();
        {register_booth, Booth} -> loop([Booth], [], [], []);
        {register, From, _} ->
            From ! {failure, no_booths},
            loop();
        {ok, _, _} -> loop()
    end.

% 1 registered booth

loop([Booth1], Reg_creds, Vote_creds, Registered) ->
    receive
        stop -> flush();
        {state, From} ->
            From ! {state,
                    {[Booth1], Reg_creds, Vote_creds, Registered, no_gen}},
            loop([Booth1], Reg_creds, Vote_creds, Registered);
        {register_booth, Booth1} ->
            loop([Booth1], Reg_creds, Vote_creds, Registered);
        {register_booth, Booth} ->
            loop([Booth, Booth1], Reg_creds, Vote_creds, Registered, now());
        {register, From, Reg_cred} ->
            case lists:member(Reg_cred, Reg_creds) of
                true ->
                    From ! {failure, registered},
                    loop([Booth1], Reg_creds, Vote_creds, Registered);
                false ->
                    Vote_cred = get_voting_credentials({Vote_creds, now()}),
                    Booth1 ! {registration, Vote_cred},
                    loop([Booth1], [Reg_cred | Reg_creds],
                         [Vote_cred | Vote_creds],
                         [{From, Vote_cred, Booth1} | Registered])
            end;
        {ok, Vote_cred, Booth1} ->
            case lists:keyfind(Vote_cred, 2, Registered) of
                false ->
                    loop([Booth1], Reg_creds, Vote_creds, Registered);
                {From, _, _} ->
                    From ! {success, {Vote_cred, Booth1}},
                    loop([Booth1], Reg_creds, Vote_creds,
                         lists:delete({From, Vote_cred, Booth1}, Registered))
            end;
        {ok, _, _} ->
            loop([Booth1], Reg_creds, Vote_creds, Registered)
    end.

% multiple registered booths

loop(Booths, Reg_creds, Vote_creds, Registered, Gen_state) ->
    receive
        stop -> flush();
        {state, From} ->
            From ! {state,
                    {Booths, Reg_creds, Vote_creds, Registered, Gen_state}},
            loop(Booths, Reg_creds, Vote_creds, Registered, Gen_state);
        {register_booth, Booth} ->
            case lists:member(Booth, Booths) of
                true ->
                    loop(Booths, Reg_creds, Vote_creds, Registered, Gen_state);
                false ->
                    loop([Booth | Booths], Reg_creds, Vote_creds, Registered,
                         Gen_state)
            end;
        {register, From, Reg_cred} ->
            case lists:member(Reg_cred, Reg_creds) of
                true ->
                    From ! {failure, registered},
                    loop(Booths, Reg_creds, Vote_creds, Registered, Gen_state);
                false ->
                    Vote_cred = get_voting_credentials({Vote_creds, now()}),
                    {N, New_gen_state} =
                        random:uniform_s(length(Booths), Gen_state),
                    Booth = lists:nth(N, Booths),
                    Booth ! {registration, Vote_cred},
                    loop(Booths, [Reg_cred | Reg_creds],
                         [Vote_cred | Vote_creds],
                         [{From, Vote_cred, Booth} | Registered], New_gen_state)
            end;
        {ok, Vote_cred, Booth} ->
            case lists:keyfind(Vote_cred, 2, Registered) of
                false ->
                    loop(Booths, Reg_creds, Vote_creds, Registered, Gen_state);
                {From, _, Booth} ->
                    From ! {success, {Vote_cred, Booth}},
                    loop(Booths, Reg_creds, Vote_creds,
                         lists:delete({From, Vote_cred, Booth}, Registered),
                         Gen_state);
                {_, _, _} ->
                    loop(Booths, Reg_creds, Vote_creds, Registered, Gen_state)
            end
    end.
