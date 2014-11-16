-module(registrar_c).
-export([stop/1, allow/1, block/1, send/1, no_send/1, state/1, state/2,
         register_booth/2, deregister_booth/2, resend/2, move/3, replace/3,
         register/2, register/3]).

stop(Registrar) ->
    Registrar ! stop,
    ok.

allow(Registrar) ->
    Registrar ! allow,
    ok.

block(Registrar) ->
    Registrar ! block,
    ok.

send(Registrar) ->
    Registrar ! send,
    ok.

no_send(Registrar) ->
    Registrar ! no_send,
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

deregister_booth(Registrar, Booth) ->
    Registrar ! {deregister_booth, Booth},
    ok.

resend(Registrar, Booth) ->
    Registrar ! {resend, Booth},
    ok.

move(Registrar, Old, New) ->
    Registrar ! {move, Old, New},
    ok.

replace(Registrar, Old, New) ->
    Registrar ! {replace, Old, New},
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