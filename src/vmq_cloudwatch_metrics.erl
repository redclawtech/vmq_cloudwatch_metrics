%% Copyright 2018 Dairon Medina Caro (me@dairon.org)
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

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

-define(APP, vmq_cloudwatch_metrics).

%% The default interval used to send the data to AWS Cloudwatch.
-define(DEFAULT_INTERVAL, 60000).

-record(metric_def,
        {type        :: atom(),
         labels      :: [{atom(), string()}],
         id          :: atom() | {atom(), non_neg_integer() | atom()},
         name        :: atom(),
         description :: undefined | binary()}).

-record(state, {
    namespace  :: string(),     %% The Cloudwatch Metrics namespace.
    config     :: aws_config(), %% AWS config.
    interval   :: non_neg_integer()
}).

-type state() :: #state{}.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> ignore | {error, Reason :: term()} | {'ok', pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


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
    {ok, Enabled} = application:get_env(?APP, cloudwatch_enabled),
    case Enabled of
        true ->
            Interval = application:get_env(?APP, interval, ?DEFAULT_INTERVAL),
            {ok, Region} = application:get_env(?APP, aws_region),
            {ok, AccessKeyID} = application:get_env(?APP, aws_access_key_id),
            {ok, SecretAccessKey} = application:get_env(?APP, aws_secret_access_key),
            {ok, Namespace} = application:get_env(?APP, namespace),
            Profile = list_to_atom( os:getenv( "PROFILE", "default" ) ),
            AWSConfig = case has_config_credentials(AccessKeyID, SecretAccessKey) of
                true ->
                    lager:info("AWS credentials configured"),
                    Conf = erlcloud_mon:new(AccessKeyID, SecretAccessKey),
                    Conf;
                false ->
                %% This will attempt to fetch the AWS access key and secret automatically.
                lager:info("AWS credentials not configured, attempting to get them automatically..."),
                {ok, Conf} = erlcloud_aws:auto_config([{profile, Profile}]),
                Conf
            end,
            lager:info("The AWS config is: ~p", [AWSConfig]),
            CloudWatchConfig = erlcloud_aws:service_config(<<"mon">>, Region, AWSConfig),
            schedule_report(Interval),
            {ok, #state{namespace = Namespace,
                        config = CloudWatchConfig,
                        interval = Interval}};
        false ->
            {ok, #state{}}
    end.

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
handle_info(report, State = #state{namespace = Namespace,
                                   config = Config, interval = Interval}) ->
    lager:debug("Sending metrics to cloudwatch."),
    ServerMetrics = vmq_metrics:metrics(#{aggregate => false}),
    %% Metrics come in chunks of 20 items due to AWS limitation.
    Metrics = build_metric_datum(ServerMetrics, []),
    lists:foreach(
        fun(Chunk) ->
            erlcloud_mon:put_metric_data(Namespace, Chunk, Config)
        end, Metrics),
    schedule_report(Interval),
    {noreply, State}.


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
%% https://docs.aws.amazon.com/AmazonCloudWatch/latest/
%% APIReference/API_MetricDatum.html
%% @end
%%--------------------------------------------------------------------
build_metric_datum([{#metric_def{type=Type, name=Name}, Val} | Metrics], Acc) ->
        Metric = #metric_datum{
            metric_name = atom_to_list(Name),
            dimensions  = [
                #dimension{name = "node", value = atom_to_list(node())}
            ],
            unit = unit({Type, Name}),
            value = value(Val)
        },
        build_metric_datum(Metrics, [Metric|Acc]);
build_metric_datum([], Acc) ->
    %% Split the metrics in sublists due to CloudWatch `PutMetricData`
    %% limitation of 20 metrics per call.
    %% https://docs.aws.amazon.com/AmazonCloudWatch/latest/
    %% APIReference/API_PutMetricData.html
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determines whether or not AWS Credentials are provided in the
%% VerneMQ config.
%% @end
%%--------------------------------------------------------------------
-spec has_config_credentials(iodata(), iodata()) -> boolean().
has_config_credentials(undefined, _) ->
    false;
has_config_credentials(_, undefined) ->
    false;
has_config_credentials(undefined, undefined) ->
    false;
has_config_credentials(AccessKeyID, SecretAccessKey) when
is_list(AccessKeyID), is_list(SecretAccessKey) ->
    case {AccessKeyID, SecretAccessKey} of
        {"", ""} -> false;
        _ -> true
    end.

-spec schedule_report(non_neg_integer()) -> reference().
schedule_report(Interval) ->
    erlang:send_after(Interval, self(), report).
