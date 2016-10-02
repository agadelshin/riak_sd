-module(riak_sd_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
    VMaster = { riak_sd_vnode_master,
                  {riak_core_vnode_master, start_link, [riak_sd_vnode]},
                  permanent, 5000, worker, [riak_core_vnode_master]},

    CoordFSMs = {riak_sd_coord_sup,
              {riak_sd_coord_sup, start_link, []},
              permanent, infinity, supervisor, [riak_sd_coord_sup]},

    { ok,
        { {one_for_one, 5, 10},
          [VMaster, CoordFSMs]}}.
