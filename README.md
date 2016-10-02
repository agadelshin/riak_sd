riak_sd: A Riak Core Application
======================================

Application Structure
---------------------

This is a blank riak core application. To get started, you'll want to edit the
following files:

* `src/riak_riak_sd_vnode.erl`
  * Implementation of the riak_core_vnode behaviour
* `src/riak_sd.erl`
  * Public API for interacting with your vnode
# riak_sd

Building
--------
Make sure you have Erlang 17+ installed and operational. You also will need a working C++ compiler and GNU make.  Clone the repo.

    $ cd riak_sd
    $ make devrel
  
Building a cluster
------------------

    $ for d in dev/*; do $d/bin/riak_sd start; done
    $ dev/dev2/bin/riak_sd-admin cluster join riak_sd1@127.0.0.1
    $ dev/dev3/bin/riak_sd-admin cluster join riak_sd1@127.0.0.1
    $ dev/dev4/bin/riak_sd-admin cluster join riak_sd1@127.0.0.1
    $ dev/dev1/bin/riak_sd-admin cluster plan
    $ dev/dev1/bin/riak_sd-admin cluster commit
    $ dev/dev1/bin/riak_sd-admin member-status

Running
-------

    $ dev/dev1/bin/riak_sd attach
    riak_sd1@127.0.0.1:1> riak_sd:store(<<"foo">>, <<"bar">>).
    riak_sd1@127.0.0.1:1> riak_sd:fetch(<<"foo">>).
    riak_sd1@127.0.0.1:1> riak_sd:remove(<<"foo">>).
