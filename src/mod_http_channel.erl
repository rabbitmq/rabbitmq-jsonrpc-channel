-module(mod_http_channel).

-behaviour(application).
-export([start/2,stop/1]).

start(_Type, _StartArgs) ->
    mod_http_web:install_static(?MODULE),
    {ok, Pid} = gen_server:start_link(rabbit_http, [], []),
    Service = rfc4627_jsonrpc:service(<<"rabbitmq">>,
				      <<"urn:uuid:f98a4235-20a9-4321-a15c-94878a6a14f3">>,
				      <<"1.2">>,
				      [{<<"open">>, [{"username", str},
						     {"password", str},
						     {"sessionTimeout", num},
						     {"virtualHost", str}]}]),
    rfc4627_jsonrpc:register_service(Pid, Service),
    {ok, _} = rabbit_http_channel_sup:start_link(),
    {ok, Pid}.

stop(_State) ->
    ok.
