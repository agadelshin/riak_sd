-module(riak_sd_coord_sup).
-behaviour(supervisor).

-export([run/1,
		 start_link/0]).

-export([init/1]).

run(Args) ->
	supervisor:start_child(?MODULE, Args).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Coordinator = {undefined, {riak_sd_coord, start_link, []}, temporary, 5000, worker, [riak_sd_coord]},
	{ok, {{simple_one_for_one, 10, 10}, [Coordinator]}}.