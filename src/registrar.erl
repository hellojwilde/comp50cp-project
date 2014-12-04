-module(registrar).
-export([init/0, init/1, stop/1, state/1, state/2, register_booth/2, register/2,
         register/3, registered/2]).

% exported functions

init() -> spawn(fun() -> loop() end).

init(Node) -> spawn(Node, fun() -> loop() end).

stop(Registrar) ->
    Registrar ! stop,
    ok.

state(Registrar) -> state(Registrar, infinity).

state(Registrar, Timeout) ->
    Registrar ! {state, self()},
    receive
        {state, State, Registrar} -> State
    after
        Timeout -> {error, "Timed out waiting for registrar state."}
    end.

register_booth(Registrar, Booth) ->
    Registrar ! {register_booth, Booth},
    ok.

register(Registrar, Credentials) -> register(Registrar, Credentials, infinity).

register(Registrar, Credentials, Timeout) ->
    Registrar ! {register, self(), Credentials},
    receive
        {success, _Message, Registrar} -> _Message;
        {failure, _Message, Registrar} -> {failure, _Message}
    after
        Timeout -> {error, "Timed out waiting for registration response."}
    end.

registered(Registrar, Credentials) ->
    Registrar ! {registered, Credentials, self()},
    ok.

% other functions

get_voting_credentials(Args) ->
    {VoteCreds, State} = Args,
    Pass = random_pass(16, [], State),
    case lists:member(Pass, VoteCreds) of
        true -> get_voting_credentials({VoteCreds, now()});
        false -> Pass
    end.

random_pass(0, _Pass, _) -> _Pass;
random_pass(N, Pass, State) ->
    {C, NewState} = random_alpha_num(State),
    random_pass(N - 1, [C | Pass], NewState).

random_alpha_num(State) ->
    {N, NewState} = random:uniform_s(62, State),
    C = if
        N < 11 -> N + 47;
        N < 37 -> N + 54;
        true -> N + 60
    end,
    {C, NewState}.

% 0 registered booths

loop() ->
    receive
        stop -> ok;
        {state, From} ->
            From ! {state, {[], [], [], [], random:seed0()}, self()},
            loop();
        {register_booth, Booth} -> loop([Booth], [], [], []);
        {register, From, _} ->
            From ! {failure, no_booths, self()},
            loop();
        {registered, _, _} -> loop();
        _ -> loop()
    end.

% 1 registered booth

loop([Booth1], RegCreds, VoteCreds, Registered) ->
    receive
        stop -> ok;
        {state, From} ->
            From ! {state, {[Booth1], RegCreds, VoteCreds, Registered,
                            random:seed0()}, self()},
            loop([Booth1], RegCreds, VoteCreds, Registered);
        {register_booth, Booth1} ->
            loop([Booth1], RegCreds, VoteCreds, Registered);
        {register_booth, Booth} ->
            loop([Booth, Booth1], RegCreds, VoteCreds, Registered, now());
        {register, From, RegCred} ->
            case lists:member(RegCred, RegCreds) of
                true ->
                    From ! {failure, registered, self()},
                    loop([Booth1], RegCreds, VoteCreds, Registered);
                false ->
                    VoteCred = get_voting_credentials({VoteCreds, now()}),
                    Booth1 ! {registration, self(), VoteCred},
                    loop([Booth1], [RegCred | RegCreds],
                         [VoteCred | VoteCreds],
                         [{From, VoteCred, Booth1} | Registered])
            end;
        {registered, VoteCred, Booth1} ->
            case lists:keyfind(VoteCred, 2, Registered) of
                false ->
                    loop([Booth1], RegCreds, VoteCreds, Registered);
                {From, _, _} ->
                    From ! {success, {VoteCred, Booth1}, self()},
                    loop([Booth1], RegCreds, VoteCreds,
                         lists:delete({From, VoteCred, Booth1}, Registered))
            end;
        {registered, _, _} ->
            loop([Booth1], RegCreds, VoteCreds, Registered);
        _ -> loop([Booth1], RegCreds, VoteCreds, Registered)
    end.

% multiple registered booths

loop(Booths, RegCreds, VoteCreds, Registered, GenState) ->
    receive
        stop -> ok;
        {state, From} ->
            From ! {state,
                    {Booths, RegCreds, VoteCreds, Registered, GenState},
                    self()},
            loop(Booths, RegCreds, VoteCreds, Registered, GenState);
        {register_booth, Booth} ->
            case lists:member(Booth, Booths) of
                true ->
                    loop(Booths, RegCreds, VoteCreds, Registered, GenState);
                false ->
                    loop([Booth | Booths], RegCreds, VoteCreds, Registered,
                         GenState)
            end;
        {register, From, RegCred} ->
            case lists:member(RegCred, RegCreds) of
                true ->
                    From ! {failure, registered, self()},
                    loop(Booths, RegCreds, VoteCreds, Registered, GenState);
                false ->
                    VoteCred = get_voting_credentials({VoteCreds, now()}),
                    {N, NewGenState} =
                        random:uniform_s(length(Booths), GenState),
                    Booth = lists:nth(N, Booths),
                    Booth ! {registration, self(), VoteCred},
                    loop(Booths, [RegCred | RegCreds],
                         [VoteCred | VoteCreds],
                         [{From, VoteCred, Booth} | Registered], NewGenState)
            end;
        {registered, VoteCred, Booth} ->
            case lists:keyfind(VoteCred, 2, Registered) of
                false ->
                    loop(Booths, RegCreds, VoteCreds, Registered, GenState);
                {From, _, Booth} ->
                    From ! {success, {VoteCred, Booth}, self()},
                    loop(Booths, RegCreds, VoteCreds,
                         lists:delete({From, VoteCred, Booth}, Registered),
                         GenState);
                {_, _, _} ->
                    loop(Booths, RegCreds, VoteCreds, Registered, GenState)
            end;
        _ -> loop(Booths, RegCreds, VoteCreds, Registered, GenState)
    end.