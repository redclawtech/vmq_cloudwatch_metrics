-module(vmq_cloudwatch_metrics_cli).
-export([register_cli/0]).
-behaviour(clique_handler).

register_cli() ->
    register_config().

register_config() ->
    ConfigKeys = [
     "vmq_cloudwatch_metrics.cloudwatch_enabled",
     "vmq_cloudwatch_metrics.interval",
     "vmq_cloudwatch_metrics.namespace",
     "vmq_cloudwatch_metrics.aws_access_key_id",
     "vmq_cloudwatch_metrics.aws_secret_access_key",
     "vmq_cloudwatch_metrics.aws_region"
    ],
    [clique:register_config([Key],
        fun register_config_callback/3) || Key <- ConfigKeys
    ],
    ok = clique:register_config_whitelist(ConfigKeys).

register_config_callback(_, _, _) ->
    ok.
