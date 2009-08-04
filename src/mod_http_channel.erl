-module(mod_http_channel).

-behaviour(application).
-export([start/2,stop/1]).

start(_Type, _StartArgs) ->
    mod_http:register_static_context("mod_http_channel", ?MODULE, "priv/www"),
    mod_http:register_context_handler("rpc",
                                      fun(Req) ->
                                        case rfc4627_jsonrpc_mochiweb:handle("/rpc", Req) of
                                          no_match ->
                                            Req:not_found();
                                          {ok, Response} ->
                                            Req:respond(Response)
                                        end
                                      end),
    rabbit_http_sup:start_link().

stop(_State) ->
    ok.
