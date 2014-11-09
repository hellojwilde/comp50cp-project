-module(tally).
-export([init/2]).

% First argument is either a module exporting
% batch/1, empty/0, combine/2, and winner/1
% or a tuple
% {Batch, Empty, Combine, Winner}
% Second argument is a function to call on each updated winner
init({_Batchfun, Empty, _Combine, _Winner} = Scheme, Update) ->
	spawn(fun() -> loop(Scheme, Update, Empty()) end);
init(Scheme, Update) ->
	SchemeTup = {
		fun Scheme:batch/1,
		fun Scheme:empty/0,
		fun Scheme:combine/2,
		fun Scheme:winner/1
	},
	init(SchemeTup, Update).

loop({Batchfun, _Empty, Combine, Winner} = Scheme, Update, Result) ->
	Update(Winner(Result)),
	receive
		{batch, Batch} ->
			NewResult = Combine(Result, Batch),
			loop(Scheme, Update, NewResult);
		{announce, Pid} ->
			Pid ! {newScheme, Batchfun, self()},
			loop(Scheme, Update, Result);
		stop -> ok
	end.
