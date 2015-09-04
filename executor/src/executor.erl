-module (executor).

-export([main/1]).


-define ( WAIT_MAX_IN_MS , 20000).

main([]) -> 
	% application:start(executor),
	BackOff = backoff:init(1000, ?WAIT_MAX_IN_MS),

	{ok, Node} = find_node_to_cluster_with(),

	io:format("Node found to cluster with : ~p~n", [Node]),

	get_status(Node, BackOff),

	timer:sleep(infinity).


find_node_to_cluster_with() ->
	case os:getenv("RABBITMQ_NODE_TO_CLUSTER_WITH") of
		false ->
			{error, no_node_configured_to_cluster_with};
		V -> {ok, V}
	end.

get_status(Node, BackOff) ->
	Result = cluster_manager:status(Node),
	
	io:format("Status : ~p~n", [Result]),

	case backoff:fail(BackOff) of
		{ ?WAIT_MAX_IN_MS, _ } ->
			io:format("Back off exhausted"),
			halt(1);
		{ _, BackOff1} -> 
			get_status(Node, BackOff1)
	end.

	
