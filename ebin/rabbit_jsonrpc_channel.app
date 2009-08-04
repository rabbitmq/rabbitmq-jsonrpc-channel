{application, rabbit_jsonrpc_channel,
 [{description, "RabbitMQ JSON-RPC Channels"},
  {vsn, "0.01"},
  {modules, [
    rabbit_jsonrpc_channel,
    rabbit_jsonrpc_channel_app,
    rabbit_jsonrpc_channel_sup
  ]},
  {registered, []},
  {mod, {rabbit_jsonrpc_channel_app, []}},
  {env, [
        {default_vhost, <<"/">>},
        {docroot, "priv/www"},
        {port, 8000}
        ]},
  {applications, [kernel, stdlib, rfc4627_jsonrpc]}]}.
