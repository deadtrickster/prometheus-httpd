%% @doc
%% Ready-to-use test function.
%% Helps to test an app actually has working metrics endpoint.
%% Configuration is a proplist(hence compatible with common test) with two keys:
%% - `metrics_port';
%% - `metrics_path'.
%% Function assumes localhost.
%% @end
-module(prometheus_httpd_ct).

-export([self_test/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TELEMETRY_METRICS_METADATA,
        [
         "# TYPE telemetry_scrape_duration_seconds summary",
         "# HELP telemetry_scrape_duration_seconds Scrape duration",
         "# TYPE telemetry_scrape_size_bytes summary",
         "# HELP telemetry_scrape_size_bytes Scrape size, not encoded",
         "# TYPE telemetry_scrape_encoded_size_bytes summary",
         "# HELP telemetry_scrape_encoded_size_bytes Scrape size, encoded"
        ]).

%% ===================================================================
%% API
%% ===================================================================

self_test(Config) ->
  Path = normalize_path(?config(metrics_path, Config)),
  Port = ?config(metrics_port, Config),
  URL = format_to_string("http://localhost:~p/~s", [Port, Path]),
  {ok, MetricsResponse} = httpc:request(URL),
  ?assertMatch(200, status(MetricsResponse)),
  MetricsCT = prometheus_text_format:content_type(),
  ExpectedMetricsCT = binary_to_list(MetricsCT),
  ?assertMatch([{"content-encoding", "identity"},
                {"content-length", ExpectedMetricsCL},
                {"content-type", ExpectedMetricsCT}|_]
               when ExpectedMetricsCL > 0, headers(MetricsResponse)),
  MetricsBody = body(MetricsResponse),
  ?assertMatch(true, all_telemetry_metrics_present(MetricsBody)).

%% ===================================================================
%% Private functions
%% ===================================================================

all_telemetry_metrics_present(Body) ->
  lists:all(fun(Metric) ->
                case re:run(Body, Metric) of
                  {match, _} -> true;
                  _ -> false
                end
            end, ?TELEMETRY_METRICS_METADATA).

status({{_, Status, _}, _, _}) ->
  Status.
body({_, _, Body}) ->
  Body.

headers({_, Headers, _}) ->
  lists:sort(Headers).

normalize_path([$/| Rest]) ->
  Rest;
normalize_path(<<"/", Rest/binary>>) ->
  Rest;
normalize_path(Path) ->
  Path.

format_to_string(Format, Args) ->
  binary_to_list(
    iolist_to_binary(
      io_lib:format(Format, Args))).
