-module(rabbit_jsonrpc_channel_test_app).

-behaviour(application).
-export([start/2,stop/1]).

start(_Type, _StartArgs) ->
    Handler = rabbit_mochiweb:static_context_handler("", ?MODULE, "priv/www-examples"),
    ok = rabbit_mochiweb:register_global_handler(Handler),
    {ok, spawn(fun loop/0)}.

stop(_State) ->
    ok.

loop() ->
  receive
    _ -> loop()
  end.
