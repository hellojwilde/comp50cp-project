-module(frontend_pids).
-export([get_registrar_pid/1, get_manager_pid/1]).

-include_lib("yaws/include/yaws_api.hrl").
-include("../include/vote.hrl").

get_registrar_pid(A) ->
  A#arg.opaque#frontend_pids.registrar_pid.

get_manager_pid(A) ->
  A#arg.opaque#frontend_pids.manager_pid.
