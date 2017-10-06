%% @doc
%% Exports Prometheus metrics via configurable endpoint.
%%
%% ### Existing httpd:<br/>
%% <pre lang="erlang">
%% {modules, [
%%    ...
%%    prometheus_httpd
%%    ...
%% ]},
%% </pre>
%%
%% ### Built-in httpd instance:<br/>
%% <pre lang="erlang">
%%   prometheus_httpd:start()
%% </pre>
%%
%% ### Telemetry metrics
%%
%% - `telemetry_scrape_duration_seconds'
%% - `telemetry_scrape_size_bytes'
%% - `telemetry_scrape_encoded_size_bytes'
%%
%% ### Configuration
%% Can be configured via `prometheus_http' key of `prometheus' app env.<br/>
%% Default configuration:
%% <pre lang="erlang">
%% {prometheus, [
%%   ...
%%   {prometheus_http, [{path, "/metrics"},
%%                      {format, auto},
%%                      {port, 8081}]},
%%   ...
%%   ]}
%% </pre>
%% @end

-module(prometheus_httpd).

-export([start/0]).

%% httpd mod callbacks
-export([do/1]).

-include_lib("inets/include/httpd.hrl").
-include("prometheus_http.hrl").

-define(SERVER_NAME, "Prometheus.io metrics.").

-behaviour(application).
-export([start/2, stop/1]).
-behaviour(supervisor).
-export([init/1]).

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc
%% Starts inets httpd server with `promtheus_httpd' module enabled.
%% Also calls `prometheus_http_impl:setup/0'.
%% @end
start() ->
  inets:start(httpd, [
                      {modules, [
                                 prometheus_httpd
                                ]},
                      {port, prometheus_http_config:port()},
                      {server_name, ?SERVER_NAME},
                      {document_root, code:priv_dir(prometheus_httpd)},
                      {server_root, code:priv_dir(prometheus_httpd)}
                     ]).

%% @private
do(Info) ->
  URI = Info#mod.request_uri,
  Headers = Info#mod.parsed_header,
  GetHeader = fun(Name, Default) ->
                  proplists:get_value(Name, Headers, Default)
              end,

  %% TODO: check method, response only to GET
  case prometheus_http_impl:reply(#request{path = URI,
                                           headers = GetHeader,
                                           registry = undefined,
                                           standalone = standalone_p(Info)}) of
    {Code, RespHeaders0, Body} ->
      ContentLength = integer_to_list(iolist_size(Body)),
      RespHeaders = RespHeaders0 ++ [{code, Code},
                                     {content_length, ContentLength}],
      {break, [{response, {response, RespHeaders, [Body]}}]};
    false ->
      {proceed, Info#mod.data}
  end.

%% ===================================================================
%% Application & supervisor callbacks
%% ===================================================================

start(_, _) ->
  prometheus_http_impl:setup(),
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_) -> ok.

init([]) ->
  {ok, {{one_for_one, 0, 1}, []}}.


%% ===================================================================
%% Private Parts
%% ===================================================================

standalone_p(#mod{config_db = ConfigDb}) ->
  case httpd_util:lookup(ConfigDb, server_name) of
    ?SERVER_NAME ->
      true;
    _ -> false
  end.
