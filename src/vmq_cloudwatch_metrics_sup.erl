%%%-------------------------------------------------------------------
%% @doc vmq_cloudwatch_metrics top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(vmq_cloudwatch_metrics_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args},
                               permanent, 5000, Type, [I]}).

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
    {ok, { {one_for_one, 5, 10},
           [?CHILD(vmq_cloudwatch_metrics, worker, [])]}}.
