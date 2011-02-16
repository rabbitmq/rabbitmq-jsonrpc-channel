{application, rabbit_jsonrpc_channel,
 [{description, "RabbitMQ JSON-RPC Channels"},
  {vsn, "%%VSN%%"},
  {modules, [
    rabbit_jsonrpc_channel,
    rabbit_jsonrpc_channel_sup,
    rabbit_jsonrpc_channel_app,
    rabbit_jsonrpc_channel_app_sup,
    rabbit_jsonrpc_channel_factory
  ]},
  {registered, []},
  {mod, {rabbit_jsonrpc_channel_app, []}},
  {env, [
        {default_vhost, <<"/">>}
        ]},
  {applications, [kernel, stdlib, rfc4627_jsonrpc]}]}.
