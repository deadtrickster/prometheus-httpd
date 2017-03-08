

# Module prometheus_httpd #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Exports Prometheus metrics via configurable endpoint.

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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#setup-0">setup/0</a></td><td>
Initializes telemetry metrics.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>
Starts inets httpd server with <code>promtheus_httpd</code> module enabled.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="setup-0"></a>

### setup/0 ###

`setup() -> any()`

Initializes telemetry metrics.<br />
*NOTE:* If you plug `prometheus_httpd` in your existing httpd instance,
you have to call this function manually.

<a name="start-0"></a>

### start/0 ###

`start() -> any()`

Starts inets httpd server with `promtheus_httpd` module enabled.
Also calls [`setup()`](#setup-0).

