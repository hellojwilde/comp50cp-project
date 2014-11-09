-module(debug).
-compile([export_all]).

% Some useful functions for testing voting schemes (and maybe other stuff)

% An "update" function that just prints values:
update(X) -> io:format("~w~n", [X]).

% To vote without worrying about credentials:
% Booth = debug:booth().
% Booth ! {vote, [Candidate1, Candidate2], NumApproved}.
% Booth can be sent other messages, which get passed on to the underlying booth.

booth() -> spawn(fun() -> booth_loop(booth:init(), 0) end).

booth_loop(Booth, N) ->
	receive
		{vote, _Candidates, _Approved} = Ballot ->
			Booth ! {registration, N},
			Booth ! {ballot, N, Ballot};
		Anything ->
			Booth ! Anything
	end,
	booth_loop(Booth, N+1).

multi_message(_Pid, _Message, 0) -> ok;
multi_message(Pid, Message, N) ->
	Pid ! Message,
	multi_message(Pid, Message, N-1).