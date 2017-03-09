-module(prometheus_httpd_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% MACROS
%% ===================================================================

-define(README, "README.md").

-define(PROMETHEUS_ACCEPT, "application/vnd.google.protobuf;"
        "proto=io.prometheus.client.MetricFamily;encoding=delimited;q=0.7,"
        "text/plain;version=0.0.4;q=0.3,"
        "application/json;schema=\"prometheus/telemetry\";version=0.0.2;q=0.2,"
        "*/*;q=0.1").

-define(TELEMETRY_METRICS_METADATA,
        [
         "# TYPE telemetry_scrape_duration_seconds summary",
         "# HELP telemetry_scrape_duration_seconds Scrape duration",
         "# TYPE telemetry_scrape_size_bytes summary",
         "# HELP telemetry_scrape_size_bytes Scrape size, not encoded",
         "# TYPE telemetry_scrape_encoded_size_bytes summary",
         "# HELP telemetry_scrape_encoded_size_bytes Scrape size, encoded"
        ]).

%% @doc All tests of this suite.
all() ->
  [
   {group, positive}
  ].

%% @doc Groups of tests
groups() ->
  [
   {positive, [sequential], [
                             prometheus_httpd_standalone,
                             prometheus_httpd_negotiation,
                             prometheus_httpd_negotiation_fail,

                             prometheus_httpd_mod
                            ]}
  ].

%% @doc Start the application.
init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(inets),
  {ok, _} = application:ensure_all_started(prometheus),
  prometheus_httpd:start(),

  inets:start(httpd, [
                      {modules, [
                                 prometheus_httpd,
                                 mod_get
                                ]},
                      {port, 8082},
                      {server_name, "my test_server_name"},
                      {document_root, code:priv_dir(prometheus_httpd)},
                      {server_root, code:priv_dir(prometheus_httpd)},
                      {mime_types,[
                                   {"html","text/html"},
                                   {"css","text/css"},
                                   {"js","application/x-javascript"}
                                  ]}
                     ]),
  Config.

%% ===================================================================
%% TESTS
%% ===================================================================

prometheus_httpd_standalone(_Config) ->
  {ok, MetricsResponse} = httpc:request("http://localhost:8081/metrics"),
  ?assertMatch(200, status(MetricsResponse)),
  MetricsCT = prometheus_text_format:content_type(),
  ExpecteMetricsCT = binary_to_list(MetricsCT),
  ?assertMatch([{"content-encoding", "gzip"},
                {"content-length", ExpectedMetricsCL},
                {"content-type", ExpecteMetricsCT}|_]
               when ExpectedMetricsCL > 0, headers(MetricsResponse)),
  MetricsBody = zlib:gunzip(body(MetricsResponse)),
  ?assertMatch(true, all_telemetry_metrics_present(MetricsBody)),

  {ok, HTMLResponse} = httpc:request("http://localhost:8081/random_path"),
  ?assertMatch(200, status(HTMLResponse)),
  ExpectedHTMLCT = "text/html",
  ?assertMatch([{"content-length", ExpectedHTMLCL},
                {"content-type", ExpectedHTMLCT}|_]
               when ExpectedHTMLCL > 0, headers(HTMLResponse)),
  Path = prometheus_httpd_config:path(),
  ?assertMatch({match, _}, re:run(body(HTMLResponse), ["href=\"", Path, "\""])).

prometheus_httpd_negotiation(_Config) ->
  {ok, TextResponse} =
    httpc:request(get, {"http://localhost:8081/metrics",
                        [{"Accept-Encoding", "deflate"}]}, [], []),
  ?assertMatch(200, status(TextResponse)),
  TextCT = prometheus_text_format:content_type(),
  ExpectedTextCT = binary_to_list(TextCT),
  ?assertMatch([{"content-encoding", "deflate"},
                {"content-length", ExpectedTextCL},
                {"content-type", ExpectedTextCT}|_]
               when ExpectedTextCL > 0, headers(TextResponse)),
  ?assert(iolist_size(body(TextResponse)) > 0),

  {ok, ProtobufResponse} =
    httpc:request(get, {"http://localhost:8081/metrics",
                        [{"Accept", ?PROMETHEUS_ACCEPT},
                         {"Accept-Encoding", "identity, sdch"}]}, [], []),
  ?assertMatch(200, status(ProtobufResponse)),
  ProtobufCT = prometheus_protobuf_format:content_type(),
  ExpectedProtobufCT = binary_to_list(ProtobufCT),
  ?assertMatch([{"content-encoding", "identity"},
                {"content-length", ExpectedProtobufCL},
                {"content-type", ExpectedProtobufCT}|_]
               when ExpectedProtobufCL > 0, headers(ProtobufResponse)),
  ?assert(iolist_size(body(ProtobufResponse)) > 0).

prometheus_httpd_negotiation_fail(_Config) ->
  {ok, IdentityResponse} =
    httpc:request(get, {"http://localhost:8081/metrics",
                        [{"Accept-Encoding", "qwe"}]}, [], []),
  ?assertMatch(200, status(IdentityResponse)),
  IdentityCT = prometheus_text_format:content_type(),
  ExpectedIdentityCT = binary_to_list(IdentityCT),
  ?assertMatch([{"content-encoding", "identity"},
                {"content-length", ExpectedIdentityCL},
                {"content-type", ExpectedIdentityCT}|_]
               when ExpectedIdentityCL > 0, headers(IdentityResponse)),

  {ok, FEResponse} =
    httpc:request(get, {"http://localhost:8081/metrics",
                        [{"Accept-Encoding", "qwe, *;q=0"}]}, [], []),
  ?assertMatch(406, status(FEResponse)),
  ?assertMatch([{"content-length", "0"},
                {"content-type", "text/html"}|_], headers(FEResponse)),

  {ok, CTResponse} =
    httpc:request(get, {"http://localhost:8081/metrics",
                        [{"Accept", "image/png"}]}, [], []),
  ?assertMatch(406, status(CTResponse)),
  ?assertMatch([{"content-length", "0"},
                {"content-type", "text/html"}|_],
               headers(CTResponse)).

prometheus_httpd_mod(_Config) ->
  {ok, MetricsResponse} = httpc:request("http://localhost:8082/metrics"),
  ?assertMatch(200, status(MetricsResponse)),
  MetricsCT = prometheus_text_format:content_type(),
  ExpecteMetricsCT = binary_to_list(MetricsCT),
  ?assertMatch([{"content-encoding", "gzip"},
                {"content-length", ExpectedMetricsCL},
                {"content-type", ExpecteMetricsCT}|_]
               when ExpectedMetricsCL > 0, headers(MetricsResponse)),
  MetricsBody = zlib:gunzip(body(MetricsResponse)),
  ?assertMatch(true, all_telemetry_metrics_present(MetricsBody)),

  {ok, HTMLResponse} = httpc:request("http://localhost:8082/index.html"),
  ?assertMatch(200, status(HTMLResponse)),
  ExpectedHTMLCT = "text/html",
  ?assertMatch([{"content-length", ExpectedHTMLCL},
                {"content-type", ExpectedHTMLCT}|_]
               when ExpectedHTMLCL > 0, headers(HTMLResponse)),
  ?assertMatch({match, _}, re:run(body(HTMLResponse), "M_E_T_R_I_C_S")),

  {ok, CTResponse} =
    httpc:request(get, {"http://localhost:8082/qwe",
                        []}, [], []),
  ?assertMatch(404, status(CTResponse)),
  ?assertMatch([{"content-length", CL404},
                {"content-type", "text/html"}|_]
               when CL404 > 0,
                    headers(CTResponse)).

%% ===================================================================
%% Private parts
%% ===================================================================

all_telemetry_metrics_present(Body) ->
  lists:all(fun(Metric) ->
                case re:run(Body, Metric) of
                  {match, _} -> true;
                  _ -> false
                end
            end, ?TELEMETRY_METRICS_METADATA).

%%% Helpers

status({{_, Status, _}, _, _}) ->
  Status.
body({_, _, Body}) ->
  Body.

headers({_, Headers, _}) ->
  lists:sort(Headers).
