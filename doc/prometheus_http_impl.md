

# Module prometheus_http_impl #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#reply-1">reply/1</a></td><td>
Render metrics.</td></tr><tr><td valign="top"><a href="#setup-0">setup/0</a></td><td>
Initializes telemetry metrics.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="reply-1"></a>

### reply/1 ###

`reply(X1) -> any()`

Render metrics

<a name="setup-0"></a>

### setup/0 ###

`setup() -> any()`

Initializes telemetry metrics.<br />
*NOTE:* If you plug `prometheus_httpd` in your existing httpd instance,
you have to call this function manually.

