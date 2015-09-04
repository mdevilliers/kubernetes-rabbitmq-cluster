-module (executor).

-export([main/1]).

-define ( WAIT_MAX_IN_MS , 300000).

main([]) -> 
	
	net_kernel:start([executor, shortnames]),
	
	{ok, Cookie} = find_cookie(),
	erlang:set_cookie(node(), Cookie),

	BackOff = backoff:init(5000, ?WAIT_MAX_IN_MS),

	{ok, NodeToCommunicateWith} = find_node_to_communicate_with(),
	{ok, Node} = find_node_to_cluster_with(),

	io:format("Node found to communicate with : ~p~n", [NodeToCommunicateWith]),
	io:format("Node found to cluster with : ~p~n", [Node]),
	io:format("Current nodes : ~p~n", [net_adm:names()]),
	io:format("Ego : ~p~n", [net_adm:localhost()]),
	io:format("Cookie : ~p~n", [erlang:get_cookie()]),

	{ok} = get_status(NodeToCommunicateWith, BackOff),

	Result = cluster_manager:stop_app(NodeToCommunicateWith),
	io:format("Stop App : ~p~n", [Result]),
	timer:sleep(5000),
	Result1 = cluster_manager:cluster_with_node(NodeToCommunicateWith, Node, ram),
	io:format("Cluster : ~p~n", [Result1]),
	timer:sleep(5000),
	Result2 = cluster_manager:start_app(NodeToCommunicateWith),
	io:format("Start App : ~p~n", [Result2]),

	timer:sleep(infinity).


find_cookie() ->
	case os:getenv("RABBITMQ_ERLANG_COOKIE") of
		false ->
			{error, no_cookie_found};
		V -> {ok, list_to_atom(V)}
	end.

find_node_to_communicate_with() ->
	case os:getenv("RABBITMQ_NODE_TO_COMMAND") of
		false ->
			{error, no_node_configured_to_cluster_with};
		V -> {ok, list_to_atom(V)}
	end.

find_node_to_cluster_with() ->
	case os:getenv("RABBITMQ_NODE_TO_CLUSTER_WITH") of
		false ->
			{error, no_node_configured_to_cluster_with};
		V -> {ok, list_to_atom(V)}
	end.

get_status(Node, BackOff) ->
	Result = cluster_manager:status(Node),
	
	io:format("Status : ~p~n", [Result]),

	case Result of
		[_,_,_,_] -> {ok};
		_ -> 
			case backoff:fail(BackOff) of
				{ ?WAIT_MAX_IN_MS, _ } ->
					io:format("Back off exhausted"), halt();
				{ SleepTime, BackOff1} -> 
					io:format("Sleeping : ~p~n", [SleepTime]),
					timer:sleep(SleepTime),
					get_status(Node, BackOff1)
			end
	end.