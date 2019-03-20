# VerneMQ CloudWatch Metrics plugin

This is a [VerneMQ](https://vernemq.com/) plugin that will send broker metrics to Amazon CloudWatch.

## Prerequisites

* A recent version of Erlang/OTP(Not tested on versions previous to 19).
* VerneMQ version *1.7.0* or superior.

## How to compile this plugin

This project uses [Rebar3](https://www.rebar3.org) as a build tool and can be compiled running:

```bash
rebar3 compile
```

## How to enable the plugin on VerneMQ

### Manually enable

```console
vmq-admin plugin enable --name=vmq_cloudwatch_metrics --path=<PathToYourPlugin>/_build/default/lib/vmq_cloudwatch_metrics
```

### Permanently enable (On VerneMQ start)

Add the following to the `vernemq.conf` file.

```erlang
plugins.vmq_cloudwatch_metrics = on
plugins.vmq_cloudwatch_metrics.path = <PathToYourPlugin>/_build/default/lib/vmq_cloudwatch_metrics
```

## Configuration

The following settings are available for this plugin:

- **vmq_cloudwatch_metrics.cloudwatch_enabled** Defines if the plugin will send the metrics to CloudWatch or not. Dafault "off".

- **vmq_cloudwatch_metrics.interval** The publish interval in milliseconds. Defaults to 60000(1 minute).

- **vmq_cloudwatch_metrics.namespace** CloudWatch namespaces are containers for metrics. Defaults to "VerneMQ".

- **vmq_cloudwatch_metrics.aws_access_key_id** The AWS Key. See http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_access-keys.html.

- **vmq_cloudwatch_metrics.aws_secret_access_key** The AWS secret key for connecting to CloudWatch. See http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_access-keys.html.

- **vmq_cloudwatch_metrics.aws_region** The AWS region. Defaults to "us-east-1".

Parameters can be defined in the `vernemq.conf` file.

```erlang
vmq_cloudwatch_metrics.cloudwatch_enabled = on
vmq_cloudwatch_metrics.interval = 60000
vmq_cloudwatch_metrics.namespace = "VerneMQ"
vmq_cloudwatch_metrics.aws_access_key_id = "MYACCESSKEY"
vmq_cloudwatch_metrics.aws_secret_access_key = "MYSECRETACCESSKEY"
vmq_cloudwatch_metrics.aws_region = "eu-west-1"
```

*Tip:* Configuration parameters can also be changed at runtime using the `vmq-admin` script.

## TODO

- [ ] Add tests.
- [ ] Add CI using Travis.
