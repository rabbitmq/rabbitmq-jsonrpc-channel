-module(rabbit_jsonrpc_channel_test_app).

-behaviour(application).
-export([start/2,stop/1]).

start(_Type, _StartArgs) ->
    rabbit_mochiweb:register_static_context("examples", ?MODULE, "priv/www-examples"),
    {ok, spawn(fun loop/0)}.

stop(_State) ->
    ok.

loop() ->
  receive
    _ -> loop()
  end.
