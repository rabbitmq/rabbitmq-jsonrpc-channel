%%   The contents of this file are subject to the Mozilla Public License
%%   Version 1.1 (the "License"); you may not use this file except in
%%   compliance with the License. You may obtain a copy of the License at
%%   http://www.mozilla.org/MPL/
%%
%%   Software distributed under the License is distributed on an "AS IS"
%%   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%%   License for the specific language governing rights and limitations
%%   under the License.
%%
%%   The Original Code is RabbitMQ.
%%
%%   The Initial Developers of the Original Code are LShift Ltd,
%%   Cohesive Financial Technologies LLC, and Rabbit Technologies Ltd.
%%
%%   Portions created before 22-Nov-2008 00:00:00 GMT by LShift Ltd,
%%   Cohesive Financial Technologies LLC, or Rabbit Technologies Ltd
%%   are Copyright (C) 2007-2008 LShift Ltd, Cohesive Financial
%%   Technologies LLC, and Rabbit Technologies Ltd.
%%
%%   Portions created by LShift Ltd are Copyright (C) 2007-2009 LShift
%%   Ltd. Portions created by Cohesive Financial Technologies LLC are
%%   Copyright (C) 2007-2009 Cohesive Financial Technologies
%%   LLC. Portions created by Rabbit Technologies Ltd are Copyright
%%   (C) 2007-2009 Rabbit Technologies Ltd.
%%
%%   All Rights Reserved.
%%
%%   Contributor(s): ______________________________________.
%%
-module(rabbit_http).
-behaviour(gen_server).

-export([start_plugin/1, stop_plugin/0]).
-export([kickstart/1, start_link/0]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).
-export([jsonrpc_amqp_invoke/2, jsonrpc_amqp_invoke/3]).


start_plugin(Terms) ->
    case lists:keysearch(rabbit_http_conf,1,Terms) of
        {value, {_,HttpdConf}} ->
            kickstart(HttpdConf);
        _ ->
            exit(bad_plugin_config)
    end.

stop_plugin() -> ok.

kickstart(HttpdConf) ->
    rfc4627_jsonrpc:start(),
    {ok, _} = httpd:start(HttpdConf),
    {ok, _} = supervisor:start_child(rabbit_sup,
				     {?MODULE,
				      {?MODULE, start_link, []},
				      transient, 100, worker, [?MODULE]}),
    {ok, _} = rabbit_http_channel_sup:start_link(),
    ok.

start_link() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    Service = rfc4627_jsonrpc:service(<<"rabbitmq">>,
				      <<"urn:uuid:f98a4235-20a9-4321-a15c-94878a6a14f3">>,
				      <<"1.2">>,
				      [{<<"open">>, [{"username", str},
						     {"password", str},
						     {"sessionTimeout", num},
						     {"virtualHost", str}]}]),
    rfc4627_jsonrpc:register_service(Pid, Service),
    {ok, Pid}.

jsonrpc_amqp_invoke(Name, Fun) ->
    jsonrpc_amqp_invoke(Name, Fun, fun (X) -> X end).

jsonrpc_amqp_invoke(Name, Fun, ErrorTransformer) ->
    case catch Fun() of
        {'EXIT', Reason = {amqp, _, _}} ->
            {_ShouldClose, Code, TextBin, Method} = rabbit_reader:lookup_amqp_exception(Reason),
            ErrorJson = {obj, [{"procedure", Name},
                               {"detail", case Method of
                                              none -> null;
                                              M when is_binary(M) -> M;
                                              M when is_list(M) -> list_to_binary(M);
                                              M when is_atom(M) -> list_to_binary(atom_to_list(M))
					  end}]},
            ErrorTransformer(rfc4627_jsonrpc:error_response(Code, TextBin, ErrorJson));
        {'EXIT', Reason} ->
            ErrorTransformer
              (rfc4627_jsonrpc:error_response(
                 500,
                 list_to_binary(lists:flatten(
                                  io_lib:format("~p: ~p",[Name, Reason]))),
                 null));
        Result ->
            Result
    end.

%% -----------------------------------------------------------------------------
%% gen_server callbacks
%% -----------------------------------------------------------------------------

init(_Args) ->
    {ok, nostate}.

handle_call({jsonrpc, <<"open">>, _RequestInfo, Args}, _From, State) ->
    {ok, Oid} = rabbit_http_channel:open(Args),
    {reply,
     {result, {obj, [{service, Oid}]}},
     State}.

handle_cast(Request, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p", [?MODULE, Request]),
    {noreply, State}.

handle_info(Info, State) ->
    error_logger:error_msg("Unhandled info in ~p: ~p", [?MODULE, Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
