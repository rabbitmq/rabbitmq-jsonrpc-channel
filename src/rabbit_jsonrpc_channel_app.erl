-module(rabbit_jsonrpc_channel_app).

-behaviour(application).
-export([start/2,stop/1]).

start(_Type, _StartArgs) ->
    ContextRoot = case application:get_env(rabbitmq_jsonrpc_channel, js_root) of
        {ok, Root} -> Root;
        undefined  -> "rabbitmq_lib"
    end,
    rabbit_mochiweb:register_static_context(jsonrpc_lib,
                                            ContextRoot, ?MODULE, "priv/www",
                                            "JSON-RPC: JavaScript library"),
    rabbit_jsonrpc_channel_app_sup:start_link().

stop(_State) ->
    rabbit_mochiweb:unregister_context(jsonrpc_lib),
    ok.
