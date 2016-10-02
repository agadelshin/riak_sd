-module(riak_sd).
-include("riak_sd.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
         ping/0,
         fetch/1,
         store/2,
         remove/1
        ]).

-ignore_xref([
              ping/0
             ]).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, riak_sd),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, riak_sd_vnode_master).


store(Key, Value) ->
    KeyHash = to_hash(Key),
    {ok, ReqId} = riak_sd_coord:coord({store, {KeyHash, Value}}, {?APPLICATION, KeyHash}, self(), {?N, ?R, ?W}),
    receive 
        {ok, ReqId} -> ok
    after 5000 -> 
        {error, timeout}
    end.

fetch(Key) ->
    KeyHash = to_hash(Key),
    {ok, ReqId} = riak_sd_coord:coord({fetch, KeyHash}, {?APPLICATION, KeyHash}, self(), {?N, ?R, ?W}),
    receive 
        {ok, ReqId, Value} -> Value
    after 5000 -> 
        {error, timeout}
    end.    

remove(Key) ->
    KeyHash = to_hash(Key),
    {ok, ReqId} = riak_sd_coord:coord({remove, KeyHash}, {?APPLICATION, KeyHash}, self(), {?N, ?R, ?W}),
    receive 
        {ok, ReqId} -> ok
    after 5000 -> 
        {error, timeout}
    end.


to_hash(Key) ->
    crypto:hash(md5, Key).