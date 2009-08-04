-module(rabbit_jsonrpc_channel_app).

-behaviour(application).
-export([start/2,stop/1]).

start(_Type, _StartArgs) ->
    rabbit_jsonrpc_channel_app_sup:start_link().

stop(_State) ->
    ok.
