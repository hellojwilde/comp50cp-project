-module(registrar_c).
-export([stop/1, state/1, state/2, register_booth/2, register/2, register/3]).

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

% registrar_vc.erl

register(Registrar, Credentials) -> register(Registrar, Credentials, infinity).

register(Registrar, Credentials, Timeout) ->
    Registrar ! {register, self(), Credentials},
    receive
        {success, _Message} -> _Message;
        {failure, _Message} -> {failure, _Message}
    after
        Timeout -> {error, "Timed out waiting for registration response."}
    end.