-module(rabbit_jsonrpc_channel_test_app).

-behaviour(application).
-export([start/2,stop/1]).

start(_Type, _StartArgs) ->
    ok = rabbit_mochiweb:register_static_context("rpc-examples", ?MODULE,
                                                 "priv/www-examples",
                                                 "JSON-RPC: examples"),
    {ok, spawn(fun loop/0)}.

stop(_State) ->
    ok.

loop() ->
  receive
    _ -> loop()
  end.
