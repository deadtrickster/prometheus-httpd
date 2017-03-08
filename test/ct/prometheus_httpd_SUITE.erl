-module(prometheus_httpd_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% MACROS
%% ===================================================================

-define(README, "README.md").

-define(EMPTY_SCRAPE_TEXT,
"# TYPE telemetry_scrape_duration_seconds summary
# HELP telemetry_scrape_duration_seconds Scrape duration
# TYPE telemetry_scrape_size_bytes summary
# HELP telemetry_scrape_size_bytes Scrape size, not encoded
# TYPE telemetry_scrape_encoded_size_bytes summary
# HELP telemetry_scrape_encoded_size_bytes Scrape size, encoded

").

%% @doc All tests of this suite.
all() ->
  [
   {group, positive}
  ].

%% @doc Groups of tests
groups() ->
  [
   {positive, [sequential], [
                             prometheus_httpd
                            ]}
  ].

%% @doc Start the application.
init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(inets),
  {ok, _} = application:ensure_all_started(prometheus),
  prometheus_httpd:start(),
  Config.

%% ===================================================================
%% TESTS
%% ===================================================================

prometheus_httpd(_Config) ->
  {ok, Response} = httpc:request("http://localhost:8081/metrics"),
  ?assertMatch(200, status(Response)),
  CT = prometheus_text_format:content_type(),
  ExpectedCT = binary_to_list(CT),
  ?assertMatch([{"content-encoding","gzip"},
                {"content-length", ExpectedCL},
                {"content-type", ExpectedCT}|_]
               when ExpectedCL > 0, headers(Response)).

%% ===================================================================
%% Private parts
%% ===================================================================

%%% Helpers

status({{_, Status, _}, _, _}) ->
  Status.
body({_, _, Body}) ->
  Body.

headers({_, Headers, _}) ->
  lists:sort(Headers).
