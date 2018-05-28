-module(vmq_cloudwatch_metrics).

-behaviour(gen_server).

-include_lib("erlcloud/include/erlcloud_aws.hrl").
-include_lib("erlcloud/include/erlcloud_mon.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-define(APP, vmq_cloudwatch_metrics).

%% The default interval used to send the data to AWS Cloudwatch.
-define(DEFAULT_INTERVAL, 20000).

-record(state, {
    namespace  :: string(),    %% The Cloudwatch Metrics namespace.
    config     :: aws_config() %% AWS config.
}).

-type state() :: #state{}.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> ignore | {error, Reason :: term()} | {'ok', pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    {ok, AccessKeyID} = application:get_env(?APP, aws_access_key_id),
    {ok, SecretAccessKey} = application:get_env(?APP, aws_secret_access_key),
    {ok, Region} = application:get_env(?APP, aws_region),
    {ok, Namespace} = application:get_env(?APP, namespace),
    Host = "monitoring." ++ Region ++ ".amazonaws.com",
    Config = erlcloud_mon:new(AccessKeyID, SecretAccessKey, Host),
    {ok, #state{namespace = Namespace, config = Config}, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: state()) ->
    {reply, Reply :: term(), NewState :: state()} |
    {reply, Reply :: term(), NewState :: state(), timeout() | hibernate} |
    {noreply, NewState :: state()} |
    {noreply, NewState :: state(), timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: state()} |
    {stop, Reason :: term(), NewState :: state()}).
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: state()) ->
    {noreply, NewState :: state()} |
    {noreply, NewState :: state(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: state()}).
handle_cast(_Msg, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, State = #state{namespace = Namespace, config = Config}) ->
    {ok, Enabled} = application:get_env(?APP, cloudwatch_enabled),
    case Enabled of
        true ->
            lager:debug("Sending metrics to cloudwatch Namespace: ~p", [Namespace]),
            Interval = application:get_env(?APP, interval, ?DEFAULT_INTERVAL),
            ServerMetrics = vmq_metrics:metrics(),
            %% Metrics come in chunks of 20 items due to AWS limitation.
            Metrics = build_metric_datum(ServerMetrics, []),
            lists:foreach(
                fun(MetricsChunk) ->
                    erlcloud_mon:put_metric_data(Namespace, MetricsChunk, Config)
                end, Metrics),
            {noreply, State, Interval};
        false ->
            %% Cloudwatch reporting not enabled
            %% Retry in 5 seconds.
            {noreply, State, 5000}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: state()) -> term()).
terminate(_Reason, _State) ->
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: state(),
    Extra :: term()) ->
    {ok, NewState :: state()} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds a list of CloudWatch `MetricDatum` objects from VerneMQ metrics.
%% https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_MetricDatum.html
%% @end
%%--------------------------------------------------------------------
build_metric_datum([{Type, Name, Val} | Metrics], Acc) ->
        Metric = #metric_datum{
            metric_name = atom_to_list(Name),
            dimensions  = [#dimension{name = "node", value = atom_to_list(node())}],
            unit = unit({Type, Name}),
            value = value(Val)
        },
        build_metric_datum(Metrics, [Metric|Acc]);
build_metric_datum([], Acc) ->
    %% Split the metrics list in sublists due to AWS CloudWatch `PutMetricData` limitation
    %% of 20 metrics per call. 
    %% See: https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_PutMetricData.html
    split(Acc, 20).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Partitions a list on several lists of N elements.
%% @end
%%--------------------------------------------------------------------
-spec split(list(), integer()) -> list().
split([], _) -> [];
split(L, N) when length(L) < N -> [L];
split(L, N) ->
    {A, B} = lists:split(N, L),
    [A | split(B, N)].

%%------------------------------------------------------------------------------
%% @private
%% @doc Determine the unit of the metric according to AWS possible values.
%%
%% Valid Values:
%%   Seconds | Microseconds | Milliseconds | Bytes | Kilobytes | Megabytes
%%  | Gigabytes | Terabytes | Bits | Kilobits | Megabits | Gigabits | Terabits
%%  | Percent | Count | Bytes/Second | Kilobytes/Second | Megabytes/Second
%%  | Gigabytes/Second | Terabytes/Second | Bits/Second | Kilobits/Second
%%  | Megabits/Second | Gigabits/Second | Terabits/Second | Count/Second | None
%% @end
%%------------------------------------------------------------------------------
-spec unit({term(), term()}) -> binary().
unit({counter, cluster_bytes_dropped}) ->
    "Bytes";
unit({counter, cluster_bytes_sent}) ->
    "Bytes";
unit({counter, cluster_bytes_received}) ->
    "Bytes";
unit({counter, bytes_sent}) ->
    "Bytes";
unit({counter, bytes_received}) ->
    "Bytes";
unit({counter, system_io_in}) ->
    "Bytes";
unit({counter, system_io_out}) ->
    "Bytes";
unit({counter, router_memory}) ->
    "Bytes";
unit({counter, retain_memory}) ->
    "Bytes";
unit({counter, system_wallclock}) ->
    "Milliseconds";
unit({gauge, system_utilization}) ->
    "Percent";
unit({gauge, <<"system_utilization_scheduler_", _Number/binary>>}) ->
    "Percent";
unit({_Type, _Metric}) ->
    "Count".

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts the metric value to float.
%% @end
%%--------------------------------------------------------------------
-spec value(any()) -> float().
value(V) when is_integer(V) -> float(V);
value(V) when is_float(V)   -> V;
value(_) -> 0.0.
