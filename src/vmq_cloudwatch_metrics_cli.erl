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
     "vmq_cloudwatch_metrics.aws_region",
     "vmq_cloudwatch_metrics.aws_profile"
    ],
    [clique:register_config([Key],
        fun register_config_callback/3) || Key <- ConfigKeys
    ],
    ok = clique:register_config_whitelist(ConfigKeys).

register_config_callback(_, _, _) ->
    ok.
