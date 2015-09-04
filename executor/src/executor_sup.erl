%%%-------------------------------------------------------------------
%% @doc executor top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('executor_sup').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->


    % ClusterManager = {cluster_manager, {cluster_manager, start_link, []},
    %                   permanent, 2000, worker, [cluster_manager]},

    {ok, { {one_for_all, 1, 5}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================
