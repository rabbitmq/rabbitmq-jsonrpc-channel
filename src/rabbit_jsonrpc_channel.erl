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
-module(rabbit_jsonrpc_channel).
-behaviour(gen_server).

-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("rabbit_common/include/rabbit_framing.hrl").

-export([open/1, start_link/2]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-define(CHANNELID, ?MODULE).

open(Args) ->
    Oid = list_to_binary(rfc4627_jsonrpc:gen_object_name()),
    {ok, Pid} = supervisor:start_child(rabbit_jsonrpc_channel_sup, [Oid, Args]),
    Service = rfc4627_jsonrpc:service(Oid,
				      <<"urn:uuid:b3f82f69-4f63-424b-8dbb-4fa53f63cf06">>,
				      <<"1.2">>,
				      [{<<"poll">>, []},
				       {<<"close">>, []},
				       {<<"call">>, [{"method", str},
						     {"args", arr}]},
				       {<<"cast">>, [{"method", str},
						     {"args", arr},
						     {"content", str},
						     {"props", arr}]}]),
    rfc4627_jsonrpc:register_service(Pid, Service),
    {ok, Oid}.

start_link(Oid, Args) ->
    gen_server:start_link(?MODULE, [Oid, Args], []).

%---------------------------------------------------------------------------

-record(state, {channel,
                collector,
		oid,
		vhost,
		realm,
		ticket,
		timeout_millisec,
		state,
		waiting_rpcs,
		waiting_polls,
		outbound}).

compute_timeout(#state{timeout_millisec = T, waiting_polls = []}) ->
    T;
compute_timeout(#state{timeout_millisec = T}) ->
    T div 2.

check_reply({result, _}) ->
    ok;
check_reply({error, _}) ->
    ok.

noreply(State0) ->
    State = check_outbound(State0),
    {noreply, State, compute_timeout(State)}.

reply(Reply, State0) ->
    check_reply(Reply),
    State = check_outbound(State0),
    {reply, Reply, State, compute_timeout(State)}.

default_param(null, Default) ->
    Default;
default_param(X, _Default) ->
    X.

release_many(_Response, []) ->
    ok;
release_many(Response, [From | Rest]) ->
    gen_server:reply(From, Response),
    release_many(Response, Rest).

release_waiters(Response, State = #state{ waiting_polls = Waiters }) ->
    release_many(Response, Waiters),
    State#state{ waiting_polls = [] }.

pop_outbound(State = #state{ outbound = Outbound }) ->
    case queue:to_list(Outbound) of
        [] ->
            {[], State};
        OutboundList ->
            {OutboundList, State#state{ outbound = queue:new() }}
    end.

empty_poll_result() ->
    {result, []}.

stop_poll_result() ->
    {result, <<"stop">>}.

check_outbound(State = #state{ waiting_polls = [] }) ->
    State;
check_outbound(State0 = #state{ waiting_polls = [LuckyWaiter | Waiting] }) ->
    case pop_outbound(State0) of
        {[], State} ->
            State;
        {OutboundList, State} ->
            gen_server:reply(LuckyWaiter, {result, OutboundList}),
            release_waiters(empty_poll_result(), State#state{ waiting_polls = Waiting })
    end.

release_collector(State = #state{ collector = none }) ->
    State;
release_collector(State = #state{ collector = CollectorPid }) ->
    ok = rabbit_queue_collector:delete_all(CollectorPid),
    State#state{ collector = none }.

final_cleanup(RpcResponse, State0) ->
    State = #state{ waiting_rpcs = WaitingRpcs } =
        release_waiters(stop_poll_result(), check_outbound(State0)),
    release_many(RpcResponse, queue:to_list(WaitingRpcs)),
    State#state{ waiting_rpcs = queue:new() }.

method_result(Command) ->
    method_result(Command, []).

method_result(Command, ExtraFields) ->
    [Method | Args] = tuple_to_list(Command),
    {obj, [{"method", list_to_binary(atom_to_list(Method))},
           {"args", Args}
           | ExtraFields]}.

do_send_command(Command, State = #state{waiting_rpcs = WaitingRpcs}) ->
    case queue:out(WaitingRpcs) of
	{{value, From}, WaitingRpcsTail} ->
	    gen_server:reply(From, {result, method_result(Command)}),
	    State#state{waiting_rpcs = WaitingRpcsTail};
	{empty, _} ->
	    rabbit_log:error("Unsolicited RPC reply: ~p~n", [Command]),
	    State
    end.

do_send_async(Command,
	      #content{properties = Props,
		       payload_fragments_rev = BodyFragmentsRev},
	      State = #state{outbound = Outbound}) ->
    ['P_basic' | PropValues] = tuple_to_list(Props),
    Result = method_result(Command,
                           [{"content", list_to_binary(lists:reverse(BodyFragmentsRev))},
                            {"props", amqp_to_js(PropValues)}]),
    check_outbound(State#state{outbound = queue:in(Result, Outbound)}).

enqueue_waiter(From, State = #state{ waiting_polls = Waiting }) ->
    State#state{ waiting_polls = [From | Waiting] }.

build_props(P = #'P_basic'{}) ->
    P;
build_props(Values) ->
    list_to_tuple(['P_basic' | js_to_amqp(Values)]).

js_to_amqp([]) -> [];
js_to_amqp([null | Rest]) -> [undefined | js_to_amqp(Rest)];
js_to_amqp([{obj, Fs} | Rest]) -> [js_table_to_amqp(Fs) | js_to_amqp(Rest)];
js_to_amqp([V | Rest]) -> [V | js_to_amqp(Rest)].

js_table_to_amqp([]) ->
    [];
js_table_to_amqp([{KeyStr, Value} | Rest]) ->
    case guess_js_table_value(Value) of
	{Type, AmqpValue} ->
	    [{list_to_binary(KeyStr), Type, AmqpValue} | js_table_to_amqp(Rest)];
	unknown ->
	    js_table_to_amqp(Rest)
    end.

guess_js_table_value(X) when is_binary(X) -> {longstr, X}; 
guess_js_table_value(X) when is_integer(X) -> {signedint, X};
guess_js_table_value({obj, Fs}) -> {table, js_table_to_amqp(Fs)};
guess_js_table_value(X) when is_float(X) -> {double, X};
guess_js_table_value(X) when is_boolean(X) -> {bool, X};
guess_js_table_value(_) -> unknown.

amqp_to_js([]) -> [];
amqp_to_js([undefined | Rest]) -> [null | amqp_to_js(Rest)];
amqp_to_js([T | Rest]) when is_list(T) -> [{obj, amqp_table_to_js(T)} | amqp_to_js(Rest)];
amqp_to_js([V | Rest]) -> [V | amqp_to_js(Rest)].

amqp_table_to_js([]) ->
    [];
amqp_table_to_js([{KeyBin, Type, Value} | Rest]) ->
    case guess_amqp_table_value(Type, Value) of
	{ok, V} ->
	    [{binary_to_list(KeyBin), V} | amqp_table_to_js(Rest)];
	unknown ->
	    amqp_table_to_js(Rest)
    end.

guess_amqp_table_value(longstr, X) -> {ok, X};
guess_amqp_table_value(signedint, X) -> {ok, X};
guess_amqp_table_value(table, X) -> {ok, {obj, amqp_table_to_js(X)}};
guess_amqp_table_value(double, X) -> {ok, X};
guess_amqp_table_value(bool, X) -> {ok, X};
guess_amqp_table_value(_, _) -> unknown.

build_content(none, _) ->
    none;
build_content(Bin, Props) ->
    #content{class_id = 60, %% basic
	     properties = build_props(Props),
	     properties_bin = none,
	     payload_fragments_rev = [Bin]}.

cast(MethodAtom, Args, Content, Props, State = #state{ channel = ChPid }) ->
    ok = rabbit_channel:do(ChPid,
			   list_to_tuple([MethodAtom | js_to_amqp(Args)]),
			   build_content(Content, Props)),
    State.

check_cast(Method, Args, Content, Props, StateBad, StateOk, K) ->
    case catch list_to_existing_atom(binary_to_list(Method)) of
	{'EXIT', {badarg, _}} ->
	    reply(rfc4627_jsonrpc:error_response(404, "AMQP method not found",
						 {obj, [{"method", Method},
							{"args", Args}]}),
		  StateBad);
	'channel.close' ->
	    %% Forbid client-originated channel.close. We wrap
	    %% channel.close in our own close method.
	    reply(rfc4627_jsonrpc:error_response(405, "AMQP method not allowed",
						 {obj, [{"method", Method}]}),
		  StateBad);
	MethodAtom ->
	    K(cast(MethodAtom,
                   Args,
                   default_param(Content, none),
                   default_param(Props, #'P_basic'{}),
                   StateOk))
    end.

%---------------------------------------------------------------------------

init([Oid, [Username, Password, SessionTimeout0, VHostPath0]]) ->
    SessionTimeout = default_param(SessionTimeout0, 10),
    {ok, DefaultVHost} = application:get_env(default_vhost),
    VHostPath = default_param(VHostPath0, DefaultVHost),

    rabbit_log:debug("HTTP Channel started, timeout ~p~n", [SessionTimeout]),
    SessionTimeoutMs = SessionTimeout * 1000,

    process_flag(trap_exit, true),

    U = rabbit_access_control:user_pass_login(Username, Password),
    ok = rabbit_access_control:check_vhost_access(U, VHostPath),
    {ok, CollectorPid} = rabbit_queue_collector:start_link(),
    {ok, ChPid} = rabbit_channel:start_link(?CHANNELID, self(), self(),
                                            Username, VHostPath, CollectorPid),

    ok = rabbit_channel:do(ChPid, #'channel.open'{}),
    {ok,
     #state{channel = ChPid,
            collector = CollectorPid,
            oid = Oid,
            vhost = VHostPath,
            timeout_millisec = SessionTimeoutMs,
            state = opening,
            waiting_rpcs = queue:new(),
            waiting_polls = [],
            outbound = queue:new()},
     SessionTimeoutMs}.

handle_call({jsonrpc, <<"poll">>, _RequestInfo, []}, From, State) ->
    noreply(enqueue_waiter(From, State));
handle_call({jsonrpc, <<"close">>, _RequestInfo, []}, _From, State0) ->
    %%% FIXME: somehow propagate some of the args into the close reason given to other callers?
    %%% FIXME: actually call channel.close?
    rabbit_log:debug("HTTP Channel closing by request.~n"),
    {OutboundList, State} = pop_outbound(State0),
    {stop, normal, {result, OutboundList}, release_collector(State)};
handle_call({jsonrpc, <<"call">>, _RequestInfo, [Method, Args]}, From,
	    State = #state{waiting_rpcs = WaitingRpcs}) ->
    check_cast(Method, Args, none, #'P_basic'{}, State, State#state{waiting_rpcs =
								    queue:in(From,
									     WaitingRpcs)},
               fun noreply/1);
handle_call({jsonrpc, <<"cast">>, _RequestInfo, [Method, Args, Content, Props]}, _From, State) ->
    check_cast(Method, Args, Content, Props, State, State,
               fun (State1) -> reply({result, []}, State1) end);
handle_call(Request, _From, State) ->
    error_logger:error_msg("Unhandled call in ~p: ~p", [?MODULE, Request]),
    reply({result, not_supported}, State).

handle_cast(Request, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p", [?MODULE, Request]),
    noreply(State).

handle_info({send_command, #'channel.open_ok'{}}, State = #state{state = opening}) ->
    noreply(State#state{state = ready});
handle_info({send_command, Command}, State) ->
    noreply(do_send_command(Command, State));
handle_info({send_command_and_notify, QPid, TxPid, Command, Content}, State) ->
    rabbit_amqqueue:notify_sent(QPid, TxPid),
    noreply(do_send_async(Command, Content, State));

handle_info(timeout, State = #state{waiting_polls = []}) ->
    rabbit_log:debug("HTTP Channel timed out, closing.~n"),
    {stop, normal, State};
handle_info(timeout, State) ->
    noreply(release_waiters(empty_poll_result(), State));

handle_info(shutdown, State) ->
    rabbit_log:debug("HTTP Channel writer shutdown requested.~n"),
    %% We're going to close pretty soon anyway. No special action needed here.
    noreply(State);

handle_info({channel_exit, _ChannelId, #amqp_error{name = ErrorName,
                                                   explanation = Explanation,
                                                   method = MethodName}},
            State) ->
    Detail = {obj, [{code, list_to_binary(atom_to_list(ErrorName))},
                    {text, list_to_binary(Explanation)},
                    {method, list_to_binary(atom_to_list(MethodName))}]},
    {stop, normal, final_cleanup(rfc4627_jsonrpc:error_response(500, "AMQP error", Detail),
                                 State)};
handle_info({'EXIT', Pid, Reason}, State) ->
    Detail = {obj, [{pid, list_to_binary(io_lib:format("~p", [Pid]))},
                    {reason, list_to_binary(io_lib:format("~p", [Reason]))}]},
    {stop, normal, final_cleanup(rfc4627_jsonrpc:error_response(500, "Internal error", Detail),
                                 State)};
handle_info(Info, State) ->
    error_logger:error_msg("Unhandled info in ~p: ~p", [?MODULE, Info]),
    noreply(State).

terminate(_Reason, State) ->
    _State1 = final_cleanup(rfc4627_jsonrpc:error_response(504, "Closed", null),
                            release_collector(State)),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
