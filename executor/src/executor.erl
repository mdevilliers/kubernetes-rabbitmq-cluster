-module (executor).

-export([main/1]).

-define (WAIT_MAX_IN_MS, 300000).

main([]) -> 
	
	net_kernel:start([executor, shortnames]),
	
	{ok, Cookie} = find_in_env("RABBITMQ_ERLANG_COOKIE"),
	erlang:set_cookie(node(), Cookie),

	BackOff = backoff:init(5000, ?WAIT_MAX_IN_MS),

	{ok, NodeToCommunicateWith} = find_in_env("RABBITMQ_NODE_TO_COMMAND"),
	{ok, Node} = find_in_env("RABBITMQ_NODE_TO_CLUSTER_WITH"),

	io:format("Node found to communicate with : ~p~n", [NodeToCommunicateWith]),
	io:format("Node found to cluster with : ~p~n", [Node]),
	io:format("Current nodes : ~p~n", [net_adm:names()]),
	io:format("Ego : ~p~n", [net_adm:localhost()]),
	io:format("Cookie : ~p~n", [erlang:get_cookie()]),

	{ok} = get_status(NodeToCommunicateWith, BackOff),

	Result = cluster_manager:stop_app(NodeToCommunicateWith),

	io:format("Stop App : ~p~n", [Result]),
	
	Result1 = cluster_manager:cluster_with_node(NodeToCommunicateWith, Node, ram),

	io:format("Cluster : ~p~n", [Result1]),

	ok = update_cluster_node_needed(Result1, NodeToCommunicateWith, Node), 

	Result2 = cluster_manager:start_app(NodeToCommunicateWith),

	io:format("Start App : ~p~n", [Result2]),

	timer:sleep(infinity).


% private
update_cluster_node_needed({error,mnesia_not_running}, NodeToCommunicateWith, Node) -> 

	io:format("Mnesia not running~n"),
	% start
	cluster_manager:start_app(NodeToCommunicateWith),

	% wait
	timer:sleep(5000),

	% stop
	cluster_manager:stop_app(NodeToCommunicateWith),

	% cluster again
	Result = cluster_manager:cluster_with_node(NodeToCommunicateWith, Node, ram),
	update_cluster_node_needed(Result, NodeToCommunicateWith,Node);

update_cluster_node_needed({ok,already_member} ,NodeToCommunicateWith, Node) -> 
	Result = cluster_manager:update_cluster_node( NodeToCommunicateWith, Node ),
	io:format("Update Cluster Node : ~p~n", [Result]),
	ok;
update_cluster_node_needed(_,_,_) -> ok.

find_in_env(Key) -> 
	case os:getenv(Key) of
		false -> {error, not_found};
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