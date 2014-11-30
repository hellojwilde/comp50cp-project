% Patterned after: http://yaws.hyber.org/code.yaws?file=/ybed_sup.erl

-module(frontend_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link(_VoteConfig) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  FrontendChildSpec = {
    frontend, 
    {frontend, start, []},
    permanent, 
    2000,
    worker,
    [frontend]
  },
  {ok, {{one_for_all, 0, 1}, [FrontendChildSpec]}}.
