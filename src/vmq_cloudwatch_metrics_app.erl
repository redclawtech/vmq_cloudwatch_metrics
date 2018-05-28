%%%-------------------------------------------------------------------
%% @doc vmq_cloudwatch_metrics public API
%% @end
%%%-------------------------------------------------------------------

-module(vmq_cloudwatch_metrics_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    vmq_cloudwatch_metrics_sup:start_link().

stop(_State) ->
    ok.