

# Module prometheus_httpd_ct #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Ready-to-use test function.

<a name="description"></a>

## Description ##
Helps to test an app actually has working metrics endpoint.
Configuration is a proplist(hence compatible with common test) with two keys:
- `metrics_port`;
- `metrics_path`.
Function assumes localhost.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#self_test-1">self_test/1</a></td><td></td></tr><tr><td valign="top"><a href="#self_test-2">self_test/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="self_test-1"></a>

### self_test/1 ###

`self_test(Config) -> any()`

<a name="self_test-2"></a>

### self_test/2 ###

`self_test(Port, Path0) -> any()`

