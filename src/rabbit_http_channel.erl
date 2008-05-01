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
%%   The Initial Developers of the Original Code are LShift Ltd.,
%%   Cohesive Financial Technologies LLC., and Rabbit Technologies Ltd.
%%
%%   Portions created by LShift Ltd., Cohesive Financial
%%   Technologies LLC., and Rabbit Technologies Ltd. are Copyright (C) 
%%   2007 LShift Ltd., Cohesive Financial Technologies LLC., and Rabbit 
%%   Technologies Ltd.; 
%%
%%   All Rights Reserved.
%%
%%   Contributor(s): ______________________________________.
%%
-module(rabbit_http_channel).
-behaviour(gen_server).

-include("rabbit.hrl").
-include("rabbit_framing.hrl").

-export([open/2, start_link/3]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

open(ModData, Args) ->
    Oid = list_to_binary(mod_jsonrpc:gen_object_name()),
    {ok, Pid} = supervisor:start_child(rabbit_http_channel_sup, [Oid, ModData, Args]),
    Service = mod_jsonrpc:service(Oid,
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
    mod_jsonrpc:register_service(Pid, Service),
    {ok, Oid}.

start_link(Oid, ModData, Args) ->
    gen_server:start_link(?MODULE, [Oid, ModData, Args], []).

%---------------------------------------------------------------------------

-record(state, {channel,
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

release_waiters([]) ->
    ok;
release_waiters([From | Rest]) ->
    gen_server:reply(From, {result, []}),
    release_waiters(Rest).

release_rpcs([]) ->
    ok;
release_rpcs([From | Rest]) ->
    gen_server:reply(From, mod_jsonrpc:error_response(504, "Closed", null)),
    release_rpcs(Rest).

reset_waiting_state(State) ->
    State#state{ outbound = queue:new(),
		 waiting_polls = [] }.

check_outbound(State = #state{ waiting_polls = [] }) ->
    State;
check_outbound(State = #state{ waiting_polls = [LuckyWaiter | Waiting], outbound = Outbound }) ->
    OutboundList = queue:to_list(Outbound),
    if
        OutboundList == [] ->
            State;
        true ->
            gen_server:reply(LuckyWaiter, {result, OutboundList}),
            release_waiters(Waiting),
            reset_waiting_state(State)
    end.

do_send_command(Method, Args, State = #state{waiting_rpcs = WaitingRpcs}) ->
    case queue:out(WaitingRpcs) of
	{{value, From}, WaitingRpcsTail} ->
	    gen_server:reply(From, {result, {obj, [{"method", list_to_binary(atom_to_list(Method))},
						   {"args", Args}]}}),
	    State#state{waiting_rpcs = WaitingRpcsTail};
	{empty, _} ->
	    rabbit_log:error("Unsolicited RPC reply: ~p~n", [[Method | Args]]),
	    State
    end.

do_send_async(Method,
	      Args,
	      #content{properties = Props,
		       payload_fragments_rev = BodyFragmentsRev},
	      State = #state{outbound = Outbound}) ->
    ['P_basic' | PropValues] = tuple_to_list(Props),
    Result = {obj, [{"method", list_to_binary(atom_to_list(Method))},
		    {"args", Args},
		    {"content", list_to_binary(lists:reverse(BodyFragmentsRev))},
		    {"props", amqp_to_js(PropValues)}]},
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

check_cast(Method, Args, Content, Props, StateBad, StateOk) ->
    case catch list_to_existing_atom(binary_to_list(Method)) of
	{'EXIT', {badarg, _}} ->
	    reply(mod_jsonrpc:error_response(404, "AMQP method not found",
					     {obj, [{"method", Method},
						    {"args", Args}]}),
		  StateBad);
	MethodAtom ->
	    noreply(cast(MethodAtom,
			 Args,
			 default_param(Content, none),
			 default_param(Props, #'P_basic'{}),
			 StateOk))
    end.

%---------------------------------------------------------------------------

init([Oid, _ModData, [Username, Password, SessionTimeout0, VHostPath0]]) ->
    SessionTimeout = default_param(SessionTimeout0, 10),
    {ok, DefaultVHost} = application:get_env(default_vhost),
    VHostPath = default_param(VHostPath0, DefaultVHost),

    rabbit_log:debug("HTTP Channel started, timeout ~p~n", [SessionTimeout]),
    SessionTimeoutMs = SessionTimeout * 1000,

    process_flag(trap_exit, true),

    U = rabbit_access_control:user_pass_login(Username, Password),
    ok = rabbit_access_control:check_vhost_access(U, VHostPath),
    ChPid = rabbit_channel:start_link(self(), self(), Username, VHostPath),

    ok = rabbit_channel:do(ChPid, #'channel.open'{out_of_band = <<"">>}),
    {ok,
     reset_waiting_state(#state{channel = ChPid,
				oid = Oid,
				vhost = VHostPath,
				timeout_millisec = SessionTimeoutMs,
				state = opening,
				waiting_rpcs = queue:new()}),
     SessionTimeoutMs}.

handle_call({jsonrpc, <<"poll">>, _ModData, []}, From, State) ->
    noreply(enqueue_waiter(From, State));
handle_call({jsonrpc, <<"close">>, _ModData, []},
	    _From,
	    State = #state{outbound = Outbound,
			   waiting_rpcs = WaitingRpcs,
			   waiting_polls = WaitingPolls}) ->
    rabbit_log:debug("HTTP Channel closing by request.~n"),
    release_rpcs(queue:to_list(WaitingRpcs)),
    release_waiters(WaitingPolls),
    {stop, normal, {result, queue:to_list(Outbound)}, reset_waiting_state(State)};
handle_call({jsonrpc, <<"call">>, _ModData, [Method, Args]}, From,
	    State = #state{waiting_rpcs = WaitingRpcs}) ->
    check_cast(Method, Args, none, #'P_basic'{}, State, State#state{waiting_rpcs =
								    queue:in(From,
									     WaitingRpcs)});
handle_call({jsonrpc, <<"cast">>, _ModData, [Method, Args, Content, Props]}, From, State) ->
    check_cast(Method, Args, Content, Props, State, enqueue_waiter(From, State));
handle_call(Request, _From, State) ->
    error_logger:error_msg("Unhandled call in ~p: ~p", [?MODULE, Request]),
    reply({result, not_supported}, State).

handle_cast(Request, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p", [?MODULE, Request]),
    noreply(State).

handle_info({send_command, #'channel.open_ok'{}}, State = #state{state = opening}) ->
    noreply(State#state{state = ready});
handle_info({send_command, Command}, State) ->
    [Method | Args] = tuple_to_list(Command),
    noreply(do_send_command(Method, Args, State));
handle_info({send_command_and_notify, QPid, TxPid, Command, Content}, State) ->
    rabbit_amqqueue:notify_sent(QPid, TxPid),
    [Method | Args] = tuple_to_list(Command),
    noreply(do_send_async(Method, Args, Content, State));
handle_info(timeout, State = #state{waiting_polls = []}) ->
    rabbit_log:debug("HTTP Channel timed out, closing.~n"),
    {stop, normal, State};
handle_info(timeout, State = #state{waiting_polls = Waiting}) ->
    release_waiters(Waiting),
    noreply(State#state{waiting_polls = []});
handle_info(Info, State) ->
    error_logger:error_msg("Unhandled info in ~p: ~p", [?MODULE, Info]),
    noreply(State).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
