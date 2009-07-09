{application, rabbit_http,
 [{description, "Http adapter"},
  {vsn, "1"},
  {modules, [rabbit_http, rabbit_http_channel, rabbit_http_channel_sup]},
  {registered, [rabbit_http]},
  {applications, [rabbit]},
  {mod, {rabbit_http,[]}}
 ]}.

