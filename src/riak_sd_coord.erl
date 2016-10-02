-module(riak_sd_coord).
-behaviour(gen_fsm).
-include("riak_sd.hrl").

-export([start_link/5, coord/4]).
-export([init/1, code_change/4, handle_event/3, handle_info/3,
         handle_sync_event/4, terminate/3]).
-export([prepare/2, execute/2, waiting/2]).


-record(state, {req_id :: pos_integer(),
                from :: pid(),
                n :: pos_integer(),
                w :: pos_integer(),
                r :: pos_integer(),
                operation,
                key,
                accum,
                preflist :: riak_core_apl:preflist2(),
                num_w = 0 :: non_neg_integer(),
                num_r = 0 :: non_neg_integer()}).

start_link(ReqId, From, Operation, Key, Rules) ->
	gen_fsm:start_link(?MODULE, [ReqId, From, Operation, Key, Rules], []).


coord(Operation, Key, From, Rules) ->
    ReqId = reqid(),
    riak_sd_coord_sup:run([ReqId, From, Operation, Key, Rules]),
    {ok, ReqId}.

init([ReqId, From, Operation, Key, {N, R, W}]) ->
	State = #state{req_id = ReqId, from = From, operation = Operation, key = Key, n = N, r = R, w = W, accum = []},
	{ok, prepare, State, 0}.

prepare(timeout, State = #state{n = N, key = Key}) ->
	DocIdx = riak_core_util:chash_key(Key),
	PrefList = riak_core_apl:get_apl(DocIdx, N, riak_sd),
	NewState = State#state{preflist = PrefList},
	{next_state, execute, NewState, 0}.

execute(timeout, State=#state{req_id=ReqId, operation=Operation, preflist=PrefList}) ->
	riak_core_vnode_master:command(PrefList, {ReqId, Operation}, {fsm, undefined, self()}, riak_sd_vnode_master),
	{next_state, waiting, State}.

waiting(ReqId, State=#state{from = From, num_w = NumW, w = W, operation = {store,_StoreItem}}) ->
	NewNumW = NumW + 1,
	NewState = State#state{num_w=NewNumW},
	if
		NewNumW =:= W ->
			From ! {ok, ReqId},
			{stop, normal, NewState};
		true -> {next_state, waiting, NewState}
	end;
waiting({ReqId, Resp}, State=#state{from = From, num_r = NumR, r = R, operation = {fetch,_FetchItem}, accum = Accum}) ->
	NewNumR = NumR + 1,
	NewAccum = [Resp|Accum],
	NewState = State#state{num_r=NewNumR, accum=NewAccum},
	if
		NewNumR =:= R ->
			From ! {ok, ReqId, NewAccum},
			{stop, normal, NewState};
		true -> {next_state, waiting, NewState}
	end;
waiting(ReqId, State=#state{from = From, num_w = NumW, w = W, operation = {remove,_RemoveItem}}) ->
	NewNumW = NumW + 1,
	NewState = State#state{num_w=NewNumW},
	if
		NewNumW =:= W ->
			From ! {ok, ReqId},
			{stop, normal, NewState};
		true -> {next_state, waiting, NewState}
	end.

handle_info(Info, _StateName, StateData) ->
	lager:warning("got unexpected info ~p", [Info]),
	{stop,badmsg,StateData}.

handle_event(Event, _StateName, StateData) ->
	lager:warning("got unexpected event ~p", [Event]),
	{stop,badmsg,StateData}.

handle_sync_event(Event, _From, _StateName, StateData) ->
	lager:warning("got unexpected sync event ~p", [Event]),
	{stop,badmsg,StateData}.

code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

terminate(_Reason, _SN, _SD) ->
    ok.

reqid() ->
	erlang:phash2(erlang:now()).