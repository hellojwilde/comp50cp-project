-module(registrar).
-export([init/0, init/1, stop/1, allow/1, block/1, send/1, no_send/1, state/1,
         state/2, register_booth/2, deregister_booth/2, resend/2, move/3,
         replace/3, register/2, register/3]).

% exported functions

init() -> spawn(fun() -> loop(block, [], [], now(), []) end).

init(Node) -> spawn(Node, fun() -> loop(block, [], [], now(), []) end).

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

register(Registrar, Credentials) -> register(Registrar, Credentials, infinity).

register(Registrar, Credentials, Timeout) ->
    Registrar ! {register, self(), Credentials},
    receive
        {success, _Message} -> _Message;
        {failure, _Message} -> {failure, _Message}
    after
        Timeout -> {error, "Timed out waiting for registration response."}
    end.

% other functions

flush() ->
    receive
        _ -> flush()
    after
        0 -> ok
    end.

send_registrations([]) -> ok;
send_registrations([{_Reg_cred, Vote_cred, Booth} | T]) ->
    Booth ! {registration, Vote_cred},
    send_registrations(T).

get_voting_credentials(_Reg_cred) -> _Reg_cred.

get_ffun(Booth) ->
    fun(Elem) ->
        case element(3, Elem) of
            Booth -> true;
            _ -> false
        end
    end.

get_fmfun(Old, New) ->
    fun(Elem) ->
        case element(3, Elem) of
            Old -> {true, setelement(3, Elem, New)};
            _ -> false
        end
    end.

get_kmfun(Old, New) ->
    fun(Elem) ->
        case Elem of
            Old -> New;
            _ -> Elem
        end
    end.

%
% send voter registrations
% loop/4
%

% 0 registered booths

loop(_Status, [], Registered, _Gen_state) ->
    receive
        stop -> flush();
        allow -> loop(allow, [], Registered, _Gen_state);
        block -> loop(block, [], Registered, _Gen_state);
        send -> loop(_Status, [], Registered, _Gen_state);
        no_send -> loop(_Status, [], Registered, _Gen_state, []);
        {state, From} ->
            From ! {state, {_Status, [], Registered, _Gen_state, sending}},
            loop(_Status, [], Registered, _Gen_state);
        {register_booth, Booth} ->
            loop(_Status, [Booth], Registered, _Gen_state);
        {deregister_booth, _} -> loop(_Status, [], Registered, _Gen_state);
        {resend, Booth} ->
            Ffun = get_ffun(Booth),
            send_registrations(lists:filter(Ffun, Registered)),
            loop(_Status, [], Registered, _Gen_state);
        {move, Booth, Booth} ->
            Ffun = get_ffun(Booth),
            send_registrations(lists:filter(Ffun, Registered)),
            loop(_Status, [], Registered, _Gen_state);
        {move, Old, New} ->
            Fmfun = get_fmfun(Old, New),
            send_registrations(lists:filtermap(Fmfun, Registered)),
            Kmfun = get_kmfun(Old, New),
            New_registered = lists:keymap(Kmfun, 3, Registered),
            loop(_Status, [], New_registered, _Gen_state);
        {replace, Booth, Booth} ->
            Ffun = get_ffun(Booth),
            send_registrations(lists:filter(Ffun, Registered)),
            loop(_Status, [], Registered, _Gen_state);
        {replace, Old, New} ->
            Fmfun = get_fmfun(Old, New),
            send_registrations(lists:filtermap(Fmfun, Registered)),
            Kmfun = get_kmfun(Old, New),
            New_registered = lists:keymap(Kmfun, 3, Registered),
            loop(_Status, [], New_registered, _Gen_state);
        {register, From, _} ->
            From ! {failure, no_booths},
            loop(_Status, [], Registered, _Gen_state)
    end;

% 1 registered booth; allow voter registration

loop(allow, [Booth1], Registered, _Gen_state) ->
    receive
        stop -> flush();
        allow -> loop(allow, [Booth1], Registered, _Gen_state);
        block -> loop(block, [Booth1], Registered, _Gen_state);
        send -> loop(allow, [Booth1], Registered, _Gen_state);
        no_send -> loop(allow, [Booth1], Registered, _Gen_state, []);
        {state, From} ->
            From ! {state, {allow, [Booth1], Registered, _Gen_state, sending}},
            loop(allow, [Booth1], Registered, _Gen_state);
        {register_booth, Booth1} ->
            loop(allow, [Booth1], Registered, _Gen_state);
        {register_booth, Booth} ->
            loop(allow, [Booth, Booth1], Registered, _Gen_state);
        {deregister_booth, Booth1} -> loop(allow, [], Registered, _Gen_state);
        {deregister_booth, _Booth} ->
            loop(allow, [Booth1], Registered, _Gen_state);
        {resend, Booth} ->
            Ffun = get_ffun(Booth),
            send_registrations(lists:filter(Ffun, Registered)),
            loop(allow, [Booth1], Registered, _Gen_state);
        {move, Booth, Booth} ->
            Ffun = get_ffun(Booth),
            send_registrations(lists:filter(Ffun, Registered)),
            loop(allow, [Booth1], Registered, _Gen_state);
        {move, Old, New} ->
            Fmfun = get_fmfun(Old, New),
            send_registrations(lists:filtermap(Fmfun, Registered)),
            Kmfun = get_kmfun(Old, New),
            New_registered = lists:keymap(Kmfun, 3, Registered),
            loop(allow, [Booth1], New_registered, _Gen_state);
        {replace, Booth1, Booth1} ->
            Ffun = get_ffun(Booth1),
            send_registrations(lists:filter(Ffun, Registered)),
            loop(allow, [], Registered, _Gen_state);
        {replace, Booth1, New} ->
            Fmfun = get_fmfun(Booth1, New),
            send_registrations(lists:filtermap(Fmfun, Registered)),
            Kmfun = get_kmfun(Booth1, New),
            New_registered = lists:keymap(Kmfun, 3, Registered),
            loop(allow, [], New_registered, _Gen_state);
        {replace, Booth, Booth} ->
            Ffun = get_ffun(Booth),
            send_registrations(lists:filter(Ffun, Registered)),
            loop(allow, [Booth1], Registered, _Gen_state);
        {replace, Old, New} ->
            Fmfun = get_fmfun(Old, New),
            send_registrations(lists:filtermap(Fmfun, Registered)),
            Kmfun = get_kmfun(Old, New),
            New_registered = lists:keymap(Kmfun, 3, Registered),
            loop(allow, [Booth1], New_registered, _Gen_state);
        {register, From, Reg_cred} ->
            case lists:keymember(Reg_cred, 1, Registered) of
                true ->
                    From ! {failure, registered},
                    loop(allow, [Booth1], Registered, _Gen_state);
                false ->
                    Vote_cred = get_voting_credentials(Reg_cred),
                    Booth1 ! {registration, Vote_cred},
                    From ! {success, {Vote_cred, Booth1}},
                    loop(allow, [Booth1], [{Reg_cred, Vote_cred, Booth1} |
                                           Registered], _Gen_state)
            end
    end;

% 1 registered booth; block voter registration

loop(block, [Booth1], Registered, _Gen_state) ->
    receive
        stop -> flush();
        allow -> loop(allow, [Booth1], Registered, _Gen_state);
        block -> loop(block, [Booth1], Registered, _Gen_state);
        send -> loop(block, [Booth1], Registered, _Gen_state);
        no_send -> loop(block, [Booth1], Registered, _Gen_state, []);
        {state, From} ->
            From ! {state, {block, [Booth1], Registered, _Gen_state, sending}},
            loop(block, [Booth1], Registered, _Gen_state);
        {register_booth, Booth1} ->
            loop(block, [Booth1], Registered, _Gen_state);
        {register_booth, Booth} ->
            loop(block, [Booth, Booth1], Registered, _Gen_state);
        {deregister_booth, Booth1} -> loop(block, [], Registered, _Gen_state);
        {deregister_booth, _Booth} ->
            loop(block, [Booth1], Registered, _Gen_state);
        {resend, Booth} ->
            Ffun = get_ffun(Booth),
            send_registrations(lists:filter(Ffun, Registered)),
            loop(block, [Booth1], Registered, _Gen_state);
        {move, Booth, Booth} ->
            Ffun = get_ffun(Booth),
            send_registrations(lists:filter(Ffun, Registered)),
            loop(block, [Booth1], Registered, _Gen_state);
        {move, Old, New} ->
            Fmfun = get_fmfun(Old, New),
            send_registrations(lists:filtermap(Fmfun, Registered)),
            Kmfun = get_kmfun(Old, New),
            New_registered = lists:keymap(Kmfun, 3, Registered),
            loop(block, [Booth1], New_registered, _Gen_state);
        {replace, Booth1, Booth1} ->
            Ffun = get_ffun(Booth1),
            send_registrations(lists:filter(Ffun, Registered)),
            loop(block, [], Registered, _Gen_state);
        {replace, Booth1, New} ->
            Fmfun = get_fmfun(Booth1, New),
            send_registrations(lists:filtermap(Fmfun, Registered)),
            Kmfun = get_kmfun(Booth1, New),
            New_registered = lists:keymap(Kmfun, 3, Registered),
            loop(block, [], New_registered, _Gen_state);
        {replace, Booth, Booth} ->
            Ffun = get_ffun(Booth),
            send_registrations(lists:filter(Ffun, Registered)),
            loop(block, [Booth1], Registered, _Gen_state);
        {replace, Old, New} ->
            Fmfun = get_fmfun(Old, New),
            send_registrations(lists:filtermap(Fmfun, Registered)),
            Kmfun = get_kmfun(Old, New),
            New_registered = lists:keymap(Kmfun, 3, Registered),
            loop(block, [Booth1], New_registered, _Gen_state);
        {register, From, _} ->
            From ! {failure, blocked},
            loop(block, [Booth1], Registered, _Gen_state)
    end;

% multiple registered booths; allow voter registration

loop(allow, Booths, Registered, Gen_state) ->
    receive
        stop -> flush();
        allow -> loop(allow, Booths, Registered, Gen_state);
        block -> loop(block, Booths, Registered, Gen_state);
        send -> loop(allow, Booths, Registered, Gen_state);
        no_send -> loop(allow, Booths, Registered, Gen_state, []);
        {state, From} ->
            From ! {state, {allow, Booths, Registered, Gen_state, sending}},
            loop(allow, Booths, Registered, Gen_state);
        {register_booth, Booth} ->
            case lists:member(Booth, Booths) of
                true -> loop(allow, Booths, Registered, Gen_state);
                false -> loop(allow, [Booth | Booths], Registered, Gen_state)
            end;
        {deregister_booth, Booth} ->
            loop(allow, lists:delete(Booth, Booths), Registered, Gen_state);
        {resend, Booth} ->
            Ffun = get_ffun(Booth),
            send_registrations(lists:filter(Ffun, Registered)),
            loop(allow, Booths, Registered, Gen_state);
        {move, Booth, Booth} ->
            Ffun = get_ffun(Booth),
            send_registrations(lists:filter(Ffun, Registered)),
            loop(allow, Booths, Registered, Gen_state);
        {move, Old, New} ->
            Fmfun = get_fmfun(Old, New),
            send_registrations(lists:filtermap(Fmfun, Registered)),
            Kmfun = get_kmfun(Old, New),
            New_registered = lists:keymap(Kmfun, 3, Registered),
            loop(allow, Booths, New_registered, Gen_state);
        {replace, Booth, Booth} ->
            Ffun = get_ffun(Booth),
            send_registrations(lists:filter(Ffun, Registered)),
            loop(allow, lists:delete(Booth, Booths), Registered, Gen_state);
        {replace, Old, New} ->
            Fmfun = get_fmfun(Old, New),
            send_registrations(lists:filtermap(Fmfun, Registered)),
            Kmfun = get_kmfun(Old, New),
            New_registered = lists:keymap(Kmfun, 3, Registered),
            loop(allow, lists:delete(Old, Booths), New_registered, Gen_state);
        {register, From, Reg_cred} ->
            case lists:keymember(Reg_cred, 1, Registered) of
                true ->
                    From ! {failure, registered},
                    loop(allow, Booths, Registered, Gen_state);
                false ->
                    Vote_cred = get_voting_credentials(Reg_cred),
                    {N, New_gen_state} =
                        random:uniform_s(length(Booths), Gen_state),
                    Booth = lists:nth(N, Booths),
                    Booth ! {registration, Vote_cred},
                    From ! {success, {Vote_cred, Booth}},
                    loop(allow, Booths, [{Reg_cred, Vote_cred, Booth} |
                                         Registered], New_gen_state)
            end
    end;

% multiple registered booths; block voter registration

loop(block, Booths, Registered, _Gen_state) ->
    receive
        stop -> flush();
        allow -> loop(allow, Booths, Registered, _Gen_state);
        block -> loop(block, Booths, Registered, _Gen_state);
        send -> loop(block, Booths, Registered, _Gen_state);
        no_send -> loop(block, Booths, Registered, _Gen_state, []);
        {state, From} ->
            From ! {state, {block, Booths, Registered, _Gen_state, sending}},
            loop(block, Booths, Registered, _Gen_state);
        {register_booth, Booth} ->
            case lists:member(Booth, Booths) of
                true -> loop(block, Booths, Registered, _Gen_state);
                false -> loop(block, [Booth | Booths], Registered, _Gen_state)
            end;
        {deregister_booth, Booth} ->
            loop(block, lists:delete(Booth, Booths), Registered, _Gen_state);
        {resend, Booth} ->
            Ffun = get_ffun(Booth),
            send_registrations(lists:filter(Ffun, Registered)),
            loop(block, Booths, Registered, _Gen_state);
        {move, Booth, Booth} ->
            Ffun = get_ffun(Booth),
            send_registrations(lists:filter(Ffun, Registered)),
            loop(block, Booths, Registered, _Gen_state);
        {move, Old, New} ->
            Fmfun = get_fmfun(Old, New),
            send_registrations(lists:filtermap(Fmfun, Registered)),
            Kmfun = get_kmfun(Old, New),
            New_registered = lists:keymap(Kmfun, 3, Registered),
            loop(block, Booths, New_registered, _Gen_state);
        {replace, Booth, Booth} ->
            Ffun = get_ffun(Booth),
            send_registrations(lists:filter(Ffun, Registered)),
            loop(block, lists:delete(Booth, Booths), Registered, _Gen_state);
        {replace, Old, New} ->
            Fmfun = get_fmfun(Old, New),
            send_registrations(lists:filtermap(Fmfun, Registered)),
            Kmfun = get_kmfun(Old, New),
            New_registered = lists:keymap(Kmfun, 3, Registered),
            loop(block, lists:delete(Old, Booths), New_registered, _Gen_state);
        {register, From, _} ->
            From ! {failure, blocked},
            loop(block, Booths, Registered, _Gen_state)
    end.

%
% do not send voter registrations
% loop/5
%

% 0 registered booths

loop(_Status, [], Registered, _Gen_state, Unsent) ->
    receive
        stop -> flush();
        allow -> loop(allow, [], Registered, _Gen_state, Unsent);
        block -> loop(block, [], Registered, _Gen_state, Unsent);
        send ->
            send_registrations(Unsent),
            loop(_Status, [], Unsent ++ Registered, _Gen_state);
        no_send -> loop(_Status, [], Registered, _Gen_state, Unsent);
        {state, From} ->
            From ! {state, {_Status, [], Registered, _Gen_state, Unsent}},
            loop(_Status, [], Registered, _Gen_state, Unsent);
        {register_booth, Booth} ->
            loop(_Status, [Booth], Registered, _Gen_state, Unsent);
        {deregister_booth, _} ->
            loop(_Status, [], Registered, _Gen_state, Unsent);
        {resend, Booth} ->
            Ffun = get_ffun(Booth),
            send_registrations(lists:filter(Ffun, Registered)),
            loop(_Status, [], Registered, _Gen_state, Unsent);
        {move, Booth, Booth} ->
            Ffun = get_ffun(Booth),
            send_registrations(lists:filter(Ffun, Registered)),
            loop(_Status, [], Registered, _Gen_state, Unsent);
        {move, Old, New} ->
            Fmfun = get_fmfun(Old, New),
            send_registrations(lists:filtermap(Fmfun, Registered)),
            Kmfun = get_kmfun(Old, New),
            New_registered = lists:keymap(Kmfun, 3, Registered),
            New_unsent = lists:keymap(Kmfun, 3, Unsent),
            loop(_Status, [], New_registered, _Gen_state, New_unsent);
        {replace, Booth, Booth} ->
            Ffun = get_ffun(Booth),
            send_registrations(lists:filter(Ffun, Registered)),
            loop(_Status, [], Registered, _Gen_state, Unsent);
        {replace, Old, New} ->
            Fmfun = get_fmfun(Old, New),
            send_registrations(lists:filtermap(Fmfun, Registered)),
            Kmfun = get_kmfun(Old, New),
            New_registered = lists:keymap(Kmfun, 3, Registered),
            New_unsent = lists:keymap(Kmfun, 3, Unsent),
            loop(_Status, [], New_registered, _Gen_state, New_unsent);
        {register, From, _} ->
            From ! {failure, no_booths},
            loop(_Status, [], Registered, _Gen_state, Unsent)
    end;

% 1 registered booth; allow voter registration

loop(allow, [Booth1], Registered, _Gen_state, Unsent) ->
    receive
        stop -> flush();
        allow -> loop(allow, [Booth1], Registered, _Gen_state, Unsent);
        block -> loop(block, [Booth1], Registered, _Gen_state, Unsent);
        send ->
            send_registrations(Unsent),
            loop(allow, [Booth1], Unsent ++ Registered, _Gen_state);
        no_send -> loop(allow, [Booth1], Registered, _Gen_state, Unsent);
        {state, From} ->
            From ! {state, {allow, [Booth1], Registered, _Gen_state, Unsent}},
            loop(allow, [Booth1], Registered, _Gen_state, Unsent);
        {register_booth, Booth1} ->
            loop(allow, [Booth1], Registered, _Gen_state, Unsent);
        {register_booth, Booth} ->
            loop(allow, [Booth, Booth1], Registered, _Gen_state, Unsent);
        {deregister_booth, Booth1} ->
            loop(allow, [], Registered, _Gen_state, Unsent);
        {deregister_booth, _Booth} ->
            loop(allow, [Booth1], Registered, _Gen_state, Unsent);
        {resend, Booth} ->
            Ffun = get_ffun(Booth),
            send_registrations(lists:filter(Ffun, Registered)),
            loop(allow, [Booth1], Registered, _Gen_state, Unsent);
        {move, Booth, Booth} ->
            Ffun = get_ffun(Booth),
            send_registrations(lists:filter(Ffun, Registered)),
            loop(allow, [Booth1], Registered, _Gen_state, Unsent);
        {move, Old, New} ->
            Fmfun = get_fmfun(Old, New),
            send_registrations(lists:filtermap(Fmfun, Registered)),
            Kmfun = get_kmfun(Old, New),
            New_registered = lists:keymap(Kmfun, 3, Registered),
            New_unsent = lists:keymap(Kmfun, 3, Unsent),
            loop(allow, [Booth1], New_registered, _Gen_state, New_unsent);
        {replace, Booth1, Booth1} ->
            Ffun = get_ffun(Booth1),
            send_registrations(lists:filter(Ffun, Registered)),
            loop(allow, [], Registered, _Gen_state, Unsent);
        {replace, Booth1, New} ->
            Fmfun = get_fmfun(Booth1, New),
            send_registrations(lists:filtermap(Fmfun, Registered)),
            Kmfun = get_kmfun(Booth1, New),
            New_registered = lists:keymap(Kmfun, 3, Registered),
            New_unsent = lists:keymap(Kmfun, 3, Unsent),
            loop(allow, [], New_registered, _Gen_state, New_unsent);
        {replace, Booth, Booth} ->
            Ffun = get_ffun(Booth),
            send_registrations(lists:filter(Ffun, Registered)),
            loop(allow, [Booth1], Registered, _Gen_state, Unsent);
        {replace, Old, New} ->
            Fmfun = get_fmfun(Old, New),
            send_registrations(lists:filtermap(Fmfun, Registered)),
            Kmfun = get_kmfun(Old, New),
            New_registered = lists:keymap(Kmfun, 3, Registered),
            New_unsent = lists:keymap(Kmfun, 3, Unsent),
            loop(allow, [Booth1], New_registered, _Gen_state, New_unsent);
        {register, From, Reg_cred} ->
            case lists:keymember(Reg_cred, 1, Registered) orelse
                 lists:keymember(Reg_cred, 1, Unsent) of
                true ->
                    From ! {failure, registered},
                    loop(allow, [Booth1], Registered, _Gen_state, Unsent);
                false ->
                    Vote_cred = get_voting_credentials(Reg_cred),
                    From ! {success, {Vote_cred, Booth1}},
                    loop(allow, [Booth1], Registered, _Gen_state,
                         [{Reg_cred, Vote_cred, Booth1} | Unsent])
            end
    end;

% 1 registered booth; block voter registration

loop(block, [Booth1], Registered, _Gen_state, Unsent) ->
    receive
        stop -> flush();
        allow -> loop(allow, [Booth1], Registered, _Gen_state, Unsent);
        block -> loop(block, [Booth1], Registered, _Gen_state, Unsent);
        send ->
            send_registrations(Unsent),
            loop(block, [Booth1], Unsent ++ Registered, _Gen_state);
        no_send -> loop(block, [Booth1], Registered, _Gen_state, Unsent);
        {state, From} ->
            From ! {state, {block, [Booth1], Registered, _Gen_state, Unsent}},
            loop(block, [Booth1], Registered, _Gen_state, Unsent);
        {register_booth, Booth1} ->
            loop(block, [Booth1], Registered, _Gen_state, Unsent);
        {register_booth, Booth} ->
            loop(block, [Booth, Booth1], Registered, _Gen_state, Unsent);
        {deregister_booth, Booth1} ->
            loop(block, [], Registered, _Gen_state, Unsent);
        {deregister_booth, _Booth} ->
            loop(block, [Booth1], Registered, _Gen_state, Unsent);
        {resend, Booth} ->
            Ffun = get_ffun(Booth),
            send_registrations(lists:filter(Ffun, Registered)),
            loop(block, [Booth1], Registered, _Gen_state, Unsent);
        {move, Booth, Booth} ->
            Ffun = get_ffun(Booth),
            send_registrations(lists:filter(Ffun, Registered)),
            loop(block, [Booth1], Registered, _Gen_state, Unsent);
        {move, Old, New} ->
            Fmfun = get_fmfun(Old, New),
            send_registrations(lists:filtermap(Fmfun, Registered)),
            Kmfun = get_kmfun(Old, New),
            New_registered = lists:keymap(Kmfun, 3, Registered),
            New_unsent = lists:keymap(Kmfun, 3, Unsent),
            loop(block, [Booth1], New_registered, _Gen_state, New_unsent);
        {replace, Booth1, Booth1} ->
            Ffun = get_ffun(Booth1),
            send_registrations(lists:filter(Ffun, Registered)),
            loop(block, [], Registered, _Gen_state, Unsent);
        {replace, Booth1, New} ->
            Fmfun = get_fmfun(Booth1, New),
            send_registrations(lists:filtermap(Fmfun, Registered)),
            Kmfun = get_kmfun(Booth1, New),
            New_registered = lists:keymap(Kmfun, 3, Registered),
            New_unsent = lists:keymap(Kmfun, 3, Unsent),
            loop(block, [], New_registered, _Gen_state, New_unsent);
        {replace, Booth, Booth} ->
            Ffun = get_ffun(Booth),
            send_registrations(lists:filter(Ffun, Registered)),
            loop(block, [Booth1], Registered, _Gen_state, Unsent);
        {replace, Old, New} ->
            Fmfun = get_fmfun(Old, New),
            send_registrations(lists:filtermap(Fmfun, Registered)),
            Kmfun = get_kmfun(Old, New),
            New_registered = lists:keymap(Kmfun, 3, Registered),
            New_unsent = lists:keymap(Kmfun, 3, Unsent),
            loop(block, [Booth1], New_registered, _Gen_state, New_unsent);
        {register, From, _} ->
            From ! {failure, blocked},
            loop(block, [Booth1], Registered, _Gen_state, Unsent)
    end;

% multiple registered booths; allow voter registration

loop(allow, Booths, Registered, Gen_state, Unsent) ->
    receive
        stop -> flush();
        allow -> loop(allow, Booths, Registered, Gen_state, Unsent);
        block -> loop(block, Booths, Registered, Gen_state, Unsent);
        send ->
            send_registrations(Unsent),
            loop(allow, Booths, Unsent ++ Registered, Gen_state);
        no_send -> loop(allow, Booths, Registered, Gen_state, Unsent);
        {state, From} ->
            From ! {state, {allow, Booths, Registered, Gen_state, Unsent}},
            loop(allow, Booths, Registered, Gen_state, Unsent);
        {register_booth, Booth} ->
            case lists:member(Booth, Booths) of
                true -> loop(allow, Booths, Registered, Gen_state, Unsent);
                false ->
                    loop(allow, [Booth | Booths], Registered, Gen_state, Unsent)
            end;
        {deregister_booth, Booth} ->
            loop(allow, lists:delete(Booth, Booths), Registered, Gen_state,
                 Unsent);
        {resend, Booth} ->
            Ffun = get_ffun(Booth),
            send_registrations(lists:filter(Ffun, Registered)),
            loop(allow, Booths, Registered, Gen_state, Unsent);
        {move, Booth, Booth} ->
            Ffun = get_ffun(Booth),
            send_registrations(lists:filter(Ffun, Registered)),
            loop(allow, Booths, Registered, Gen_state, Unsent);
        {move, Old, New} ->
            Fmfun = get_fmfun(Old, New),
            send_registrations(lists:filtermap(Fmfun, Registered)),
            Kmfun = get_kmfun(Old, New),
            New_registered = lists:keymap(Kmfun, 3, Registered),
            New_unsent = lists:keymap(Kmfun, 3, Unsent),
            loop(allow, Booths, New_registered, Gen_state, New_unsent);
        {replace, Booth, Booth} ->
            Ffun = get_ffun(Booth),
            send_registrations(lists:filter(Ffun, Registered)),
            loop(allow, lists:delete(Booth, Booths), Registered, Gen_state,
                 Unsent);
        {replace, Old, New} ->
            Fmfun = get_fmfun(Old, New),
            send_registrations(lists:filtermap(Fmfun, Registered)),
            Kmfun = get_kmfun(Old, New),
            New_registered = lists:keymap(Kmfun, 3, Registered),
            New_unsent = lists:keymap(Kmfun, 3, Unsent),
            loop(allow, lists:delete(Old, Booths), New_registered, Gen_state,
                 New_unsent);
        {register, From, Reg_cred} ->
            case lists:keymember(Reg_cred, 1, Registered) orelse
                 lists:keymember(Reg_cred, 1, Unsent) of
                true ->
                    From ! {failure, registered},
                    loop(allow, Booths, Registered, Gen_state, Unsent);
                false ->
                    Vote_cred = get_voting_credentials(Reg_cred),
                    {N, New_gen_state} =
                        random:uniform_s(length(Booths), Gen_state),
                    Booth = lists:nth(N, Booths),
                    From ! {success, {Vote_cred, Booth}},
                    loop(allow, Booths, Registered, New_gen_state,
                         [{Reg_cred, Vote_cred, Booth} | Unsent])
            end
    end;

% multiple registered booths; block voter registration

loop(block, Booths, Registered, _Gen_state, Unsent) ->
    receive
        stop -> flush();
        allow -> loop(allow, Booths, Registered, _Gen_state, Unsent);
        block -> loop(block, Booths, Registered, _Gen_state, Unsent);
        send ->
            send_registrations(Unsent),
            loop(block, Booths, Unsent ++ Registered, _Gen_state);
        no_send -> loop(block, Booths, Registered, _Gen_state, Unsent);
        {state, From} ->
            From ! {state, {block, Booths, Registered, _Gen_state, Unsent}},
            loop(block, Booths, Registered, _Gen_state, Unsent);
        {register_booth, Booth} ->
            case lists:member(Booth, Booths) of
                true -> loop(block, Booths, Registered, _Gen_state, Unsent);
                false -> loop(block, [Booth | Booths], Registered, _Gen_state,
                              Unsent)
            end;
        {deregister_booth, Booth} ->
            loop(block, lists:delete(Booth, Booths), Registered, _Gen_state,
                 Unsent);
        {resend, Booth} ->
            Ffun = get_ffun(Booth),
            send_registrations(lists:filter(Ffun, Registered)),
            loop(block, Booths, Registered, _Gen_state, Unsent);
        {move, Booth, Booth} ->
            Ffun = get_ffun(Booth),
            send_registrations(lists:filter(Ffun, Registered)),
            loop(block, Booths, Registered, _Gen_state, Unsent);
        {move, Old, New} ->
            Fmfun = get_fmfun(Old, New),
            send_registrations(lists:filtermap(Fmfun, Registered)),
            Kmfun = get_kmfun(Old, New),
            New_registered = lists:keymap(Kmfun, 3, Registered),
            New_unsent = lists:keymap(Kmfun, 3, Unsent),
            loop(block, Booths, New_registered, _Gen_state, New_unsent);
        {replace, Booth, Booth} ->
            Ffun = get_ffun(Booth),
            send_registrations(lists:filter(Ffun, Registered)),
            loop(block, lists:delete(Booth, Booths), Registered, _Gen_state,
                 Unsent);
        {replace, Old, New} ->
            Fmfun = get_fmfun(Old, New),
            send_registrations(lists:filtermap(Fmfun, Registered)),
            Kmfun = get_kmfun(Old, New),
            New_registered = lists:keymap(Kmfun, 3, Registered),
            New_unsent = lists:keymap(Kmfun, 3, Unsent),
            loop(block, lists:delete(Old, Booths), New_registered, _Gen_state,
                 New_unsent);
        {register, From, _} ->
            From ! {failure, blocked},
            loop(block, Booths, Registered, _Gen_state, Unsent)
    end.