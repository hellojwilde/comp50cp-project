% Patterned after: https://github.com/klacke/yaws/blob/master/examples/src/
%                  server_sent_events.erl

-module(result_events).
-behaviour(gen_server).

% From: http://comments.gmane.org/gmane.comp.web.server.yaws.general/3694
-include_lib("yaws/include/yaws_api.hrl").
-include("../include/vote.hrl").

-export([out/1]).
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-record(state, {sock, yaws_pid, winner_collector_pid}).

out(A) ->
  case(A#arg.req)#http_request.method of
    'GET' ->
      case yaws_api:get_header(A#arg.headers, accept) of
        undefined -> {status, 406};
        Accept ->
          case string:str(Accept, "text/event-stream") of
            0 -> {status, 406};
            _ ->
              {ok, Pid} = gen_server:start(?MODULE, [A], []),
              yaws_sse:headers(Pid)
          end
      end;
    _ -> [{stats, 405}, {header, {"Allow", "GET"}}]
  end.

init([Arg]) ->
  process_flag(trap_exit, true),
  WinnerCollector = Arg#arg.opaque#frontend_pids.winner_collector_pid,
  {ok, #state{sock=Arg#arg.clisock, winner_collector_pid=WinnerCollector}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(Msg, State) ->
  case Msg of
    {ok, YawsPid} -> 
      WinnerCollector = State#state.winner_collector_pid,
      WinnerCollector ! {winners, self()},
      WinnerCollector ! {subscribe, self()},
      {noreply, State#state{yaws_pid=YawsPid}};
    {discard, _YawsPid} -> {stop, normal, State};
    {winner, VotingSchemeName, Winners} ->
      #state{sock=Socket} = State,
      EventData = json2:encode({struct, [
        {"type", "winner"},
        {"data", {struct, [
          {"name", VotingSchemeName},
          {"winners", {array, Winners}}
        ]}}
      ]}),
      case yaws_sse:send_events(Socket, yaws_sse:data(EventData)) of
        ok -> {noreply, State};
        {error, closed} -> {stop, normal, State};
        {error, Reason} -> {stop, Reason, State}
      end;
    {winners, Winners} ->
      #state{sock=Socket} = State,
      EventData = json2:encode({struct, [
        {"type", "winners"},
        {"data", {struct, lists:map(
          fun({Key, Value}) -> {Key, {array, Value}} end,
          maps:to_list(Winners)
        )}}
      ]}),
      case yaws_sse:send_events(Socket, yaws_sse:data(EventData)) of
        ok -> {noreply, State};
        {error, closed} -> {stop, normal, State};
        {error, Reason} -> {stop, Reason, State}
      end;
    {tcp_closed, _} -> {stop, normal, State#state{sock=closed}};
    {_Info} -> {noreply, State}
  end.

terminate(_Reason, #state{
    sock=Socket, 
    yaws_pid=YawsPid, 
    winner_collector_pid=WinnerCollector
  }) ->
  WinnerCollector ! {unsubscribe, self()},
  yaws_api:stream_process_end(Socket, YawsPid),
  ok.

code_change(_OldVersion, State, _Extra) ->
  {ok, State}.
