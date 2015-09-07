-module (cluster_manager).

-export ([status/1, cluster_with_node/3, stop_app/1, start_app/1, update_cluster_node/2, mnesia_reset/1 ]).

status(Node) ->
	rpc_call(Node, rabbit_mnesia, status, []).

cluster_with_node(Node, ClusterNode, disc) ->
	rpc_call(Node, rabbit_mnesia, join_cluster, [ClusterNode, disc]);
cluster_with_node(Node, ClusterNode, ram) ->
	rpc_call(Node, rabbit_mnesia, join_cluster, [ClusterNode, ram]).

stop_app(Node) ->
	rpc_call(Node, rabbit, stop, []).
start_app(Node) ->
	rpc_call(Node, rabbit, start, []).

update_cluster_node(Node,ClusterNode) ->
    rpc_call(Node, rabbit_mnesia, update_cluster_nodes, [ClusterNode]).

mnesia_reset(Node) ->
    rpc_call(Node, rabbit_mnesia, reset, []).

% private
rpc_call(Node, Mod, Fun, Args) ->
    rpc_call(Node, Mod, Fun, Args, infinity).

rpc_call(Node, Mod, Fun, Args, Timeout) ->
    case rpc:call(Node, net_kernel, get_net_ticktime, [], Timeout) of
        {badrpc, _} = E -> E;
        Time            -> net_kernel:set_net_ticktime(Time, 0),
                           rpc:call(Node, Mod, Fun, Args, Timeout)
    end.
