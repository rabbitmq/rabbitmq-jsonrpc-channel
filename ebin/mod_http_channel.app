{application, mod_http_channel,
 [{description, "mod_http_channel"},
  {vsn, "0.01"},
  {modules, [
    mod_http_channel,
    rabbit_http,
    rabbit_http_channel,
    rabbit_http_channel_sup
  ]},
  {registered, []},
  {mod, {mod_http_channel, []}},
  {env, [
        {docroot, "priv/www"},
        {port, 8000}
        ]},
  {applications, [kernel, stdlib, mod_http, rfc4627_jsonrpc]}]}.
