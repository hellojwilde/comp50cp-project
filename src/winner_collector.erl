-module(winner_collector).
-export([init/0, get_winner_collection_fn/2]).

init() ->
  spawn(fun () -> loop() end).

get_winner_collection_fn(VotingSchemeName, CollectorPID) ->
  fun (Winner) -> CollectorPID ! {winner, VotingSchemeName, Winner} end.

loop() ->
  loop(sets:set()).

loop(Subscribers) ->
  receive
    {winner, VotingSchemeName, Winner} -> 
      lists:foreach(
        fun (Subscriber) -> Subscriber ! {winner, VotingSchemeName, Winner} end,
        Subscribers
      ),
      loop(Subscribers);
    {subscribe, Subcriber} ->
      loop(sets:add_element(Subcriber, Subscribers));
    {unsubscribe, Subcriber} ->
      loop(sets:del_element(Subcriber, Subscribers))
  end.
