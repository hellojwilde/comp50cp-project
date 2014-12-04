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
