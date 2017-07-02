

# Prometheus.io inets httpd exporter #

Copyright (c) 2017 Ilya Khaprov <<i.khaprov@gmail.com>>.

__Version:__ 2.1.0

[![Hex.pm](https://img.shields.io/hexpm/v/prometheus_httpd.svg?maxAge=2592000?style=plastic)](https://hex.pm/packages/prometheus_httpd)
[![Hex.pm](https://img.shields.io/hexpm/dt/prometheus_httpd.svg?maxAge=2592000)](https://hex.pm/packages/prometheus_httpd)
[![Build Status](https://travis-ci.org/deadtrickster/prometheus_httpd.svg?branch=version-3)](https://travis-ci.org/deadtrickster/prometheus_httpd)
[![Coverage Status](https://coveralls.io/repos/github/deadtrickster/prometheus_httpd/badge.svg?branch=master)](https://coveralls.io/github/deadtrickster/prometheus_httpd?branch=master)

Provides [httpd middleware](http://erlang.org/doc/man/httpd.html) "mod-module" (`prometheus_httpd`) for exposing [Prometheus.io](https://github.com/deadtrickster/prometheus.erl) metrics in various formats.

Also can start its own httpd instance with just `prometheus_httpd` enabled.

## Usage

```
prometheus_httpd:start()
```

More in `prometheus_httpd` module [documentation](/deadtrickster/prometheus_httpd/blob/master/doc/prometheus_httpd.md)

![BEAM Dashboard](https://raw.githubusercontent.com/deadtrickster/beam-dashboards/master/BEAM.png)

- IRC: #erlang on Freenode; 
- [Slack](https://elixir-slackin.herokuapp.com/): #prometheus channel - [Browser](https://elixir-lang.slack.com/messages/prometheus) or App(slack://elixir-lang.slack.com/messages/prometheus).

## Integrations
- [Ecto Instrumenter](https://hex.pm/packages/prometheus_ecto)
- [Erlang client](https://github.com/deadtrickster/prometheus.erl)
- [Elixir client](https://github.com/deadtrickster/prometheus.ex)
- [Elixir plugs Instrumenters and Exporter](https://hex.pm/packages/prometheus_plugs)
- [Extatus - App to report metrics to Prometheus from Elixir GenServers](https://github.com/gmtprime/extatus)
- [Fuse plugin](https://github.com/jlouis/fuse#fuse_stats_prometheus)
- [OS process info Collector](https://hex.pm/packages/prometheus_process_collector) (linux-only)
- [Phoenix Instrumenter](https://hex.pm/packages/prometheus_phoenix)
- [RabbitMQ Exporter](https://github.com/deadtrickster/prometheus_rabbitmq_exporter).

## Dashboards

- [Beam Dashboards](https://github.com/deadtrickster/beam-dashboards).

## Blogs

- [Monitoring Elixir apps in 2016: Prometheus and Grafana](https://aldusleaf.org/monitoring-elixir-apps-in-2016-prometheus-and-grafana/)
- [A Simple Erlang Application, with Prometheus](http://markbucciarelli.com/2016-11-23_a_simple_erlang_application_with_prometheus.html).

## License

MIT


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="prometheus_http_impl.md" class="module">prometheus_http_impl</a></td></tr>
<tr><td><a href="prometheus_httpd.md" class="module">prometheus_httpd</a></td></tr></table>

