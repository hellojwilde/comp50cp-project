-module(frontend_config).
-export([get_registrar_pid/1, get_manager_pid/1, get_candidates/1]).

-include_lib("yaws/include/yaws_api.hrl").
-include("../include/vote.hrl").

get_registrar_pid(A) ->
    A#arg.opaque#frontend_config.registrar_pid.

get_manager_pid(A) ->
    A#arg.opaque#frontend_config.manager_pid.

get_candidates(A) ->
    A#arg.opaque#frontend_config.candidates.
