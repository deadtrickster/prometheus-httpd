

# Module prometheus_httpd #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Exports Prometheus metrics via configurable endpoint.

__Behaviours:__ [`application`](application.md), [`supervisor`](supervisor.md).

<a name="description"></a>

## Description ##
### Existing httpd:<br />

```erlang

  {modules, [
     ...
     prometheus_httpd
     ...
  ]},
```

### Built-in httpd instance:<br />

```erlang

    prometheus_httpd:start()
```

### Telemetry metrics

- `telemetry_scrape_duration_seconds`
- `telemetry_scrape_size_bytes`
- `telemetry_scrape_encoded_size_bytes`

### Configuration
Can be configured via `prometheus_httpd` key of `prometheus` app env.<br />
Default configuration:

```erlang

  {prometheus, [
    ...
    {prometheus_httpd, [{path, "/metrics"},
                        {format, auto},
                        {port, 8081}]},
    ...
    ]}
```
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>
Starts inets httpd server with <code>promtheus_httpd</code> module enabled.</td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`

<a name="start-0"></a>

### start/0 ###

`start() -> any()`

Starts inets httpd server with `promtheus_httpd` module enabled.
Also calls `prometheus_http_impl:setup/0`.

<a name="start-2"></a>

### start/2 ###

`start(X1, X2) -> any()`

<a name="stop-1"></a>

### stop/1 ###

`stop(X1) -> any()`

