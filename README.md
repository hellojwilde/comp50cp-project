# "presidential fanfiction"

## Setup

You'll need the following installed on your computer:

- Erlang.
- [Rebar](https://github.com/rebar/rebar), a package manager for Erlang. Get it with the appropriate system package.

You can then use rebar to install the Erlang dependencies:

    $ rebar get-deps
    $ rebar compile

## Running the Server

In order to point `erl` at all of the deps we installed via Rebar, we have to run it with:

    erl -pa ebin deps/*/ebin

Then, from there, run:

    manager:init().

You can then go to the following URL on your local machine provided port 8888 is not already bound to some other process and play with it:

    http://localhost:8888

## File Structure Overview

- `include/` - record declarations
    + `vote.hrl` - frontend server config record
- `src/` - source files
    + `vote.app.src` - tells rebar what to compile
    + `booth.erl` - routes votes between UI and talliers
    + `frontend.erl` - configures and starts YAWS
        * `frontent_config.erl` - unpacks `vote.hrl`
        * `frontend_sup.erl` - supervises YAWS started in `frontend.erl`
        * `result_events.erl` - sends events from `winner_collector.erl`
        * `winner_collector.erl` - collects winners from talliers, 
          maintaining list of most recent winners, and allowing others to
          subscribe for notifications on changes
    + `manager.erl` - starts everything, informs modules about each other
    + `registrar.erl` - assigns passwords and voting locations to others,
      prevents duplicate registations, notifies booths of valid passwords
    + `tally.erl` - implements talliers
        * `count_map.erl` - maps, whose values are numbers, which are 
          combined by adding values
        * `pairwise.erl` - computes the graph of pairwise differences 
          between candidates
        * `point_scheme.erl` - implements four schemes which assign points 
          to candidates
        * `schultze.erl` - **the best voting scheme**
        * `vote_count.erl` - counts votes
        * `all_votes.erl` - lists all votes
        * `debug.erl` - collection of debug utilities
- `www/` - YAWS view files
    + `index.yaws` - list of major pages in the web UI
    + `magic.yaws` - contains buttons to flush and simulate votes
    + `register.yaws` - contains registration form
        * `register_post.yaws` - processes registration form,
          shows success/fail messages
    + `vote.yaws` - contains vote form
        * `vote_post.yaws` - processes vote, shows success/fail messages
    + `winners.yaws` - displays winners

