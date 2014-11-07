-module(tally).
-export([init/2]).

% First argument is a module exporting empty/0, combine/2, and winner/1
% Second argument is a function to call on each updated winner
init(Scheme, Update) ->
	spawn(fun() -> loop(Scheme, Update, Scheme:empty()) end).

loop(Scheme, Update, Result) ->
	Update(Scheme:winner(Result)),
	receive
		{batch, Batch} ->
			NewResult = Scheme:combine(Result, Batch),
			loop(Scheme, Update, NewResult);
		{announce, Pid} ->
			Pid ! {newScheme, fun Scheme:batch/1, self()},
			loop(Scheme, Update, Result);
		stop -> ok
	end.
