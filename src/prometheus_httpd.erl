-module(prometheus_httpd).

-export([start/0,
         setup/0]).

%% httpd mod callbacks
-export([do/1]).

-include_lib("inets/include/httpd.hrl").

-define(SCRAPE_DURATION, telemetry_scrape_duration_seconds).
-define(SCRAPE_SIZE, telemetry_scrape_size_bytes).
-define(SCRAPE_ENCODED_SIZE, telemetry_scrape_encoded_size_bytes).

%% ===================================================================
%% Public API
%% ===================================================================

start() ->
  setup(),
  inets:start(httpd, [
                      {modules, [
                                 prometheus_httpd
                                ]},
                      {port, prometheus_httpd_config:port()},
                      {server_name, "Prometheus.io metrics."},
                      {document_root, code:priv_dir(prometheus_httpd)},
                      {server_root, code:priv_dir(prometheus_httpd)}
                     ]).

setup() ->
  Registry = default,

  ScrapeDuration = [{name, ?SCRAPE_DURATION},
                    {help, "Scrape duration"},
                    {labels, ["registry", "content_type"]},
                    {registry, Registry}],
  ScrapeSize = [{name, ?SCRAPE_SIZE},
                {help, "Scrape size, not encoded"},
                {labels, ["registry", "content_type"]},
                {registry, Registry}],
  ScrapeEncodedSize = [{name, ?SCRAPE_ENCODED_SIZE},
                       {help, "Scrape size, encoded"},
                       {labels, ["registry", "content_type", "encoding"]},
                       {registry, Registry}],

  prometheus_summary:declare(ScrapeDuration),
  prometheus_summary:declare(ScrapeSize),
  prometheus_summary:declare(ScrapeEncodedSize).

do(Info) ->
  URI = Info#mod.request_uri,
  case prometheus_httpd_config:path() of
    URI ->
      Headers = Info#mod.parsed_header,
      Accept = proplists:get_value("accept", Headers, "text/plain"),
      AcceptEncoding = proplists:get_value("accept-encoding", Headers),
      {Code, Headers, Body} = format_metrics(Accept, AcceptEncoding),
      {break, [{response, {response, Headers ++ [{code, Code}], Body}}]};
    _ ->
      {proceed, Info}
  end.

%% ===================================================================
%% Private Parts
%% ===================================================================

format_metrics(Accept, AcceptEncoding) ->
  case negotiate_format(Accept) of
    undefined ->
      {406, [], <<>>};
    Format ->
      {ContentType, Scrape} = render_format(Format),
      case negotiate_encoding(AcceptEncoding) of
        undefined ->
          {406, [], <<>>};
        Encoding ->
          encode_format(ContentType, Encoding, Scrape)
      end
  end.

negotiate_format(Accept) ->
  case prometheus_httpd_config:format() of
    auto ->
      Alternatives = prometheus_httpd_config:allowed_formats(),
      accept_header:negotiate(Accept, Alternatives);
    undefined -> undefined;
    Format0 -> Format0
  end.

negotiate_encoding(AcceptEncoding) ->
  accept_encoding_header:negotiate(AcceptEncoding, [<<"gzip">>,
                                                    <<"deflate">>,
                                                    <<"identity">>]).

render_format(Format) ->
  Registry = default,
  ContentType = Format:content_type(),

  Scrape = prometheus_summary:observe_duration(
             Registry,
             ?SCRAPE_DURATION,
             [Registry, ContentType],
             fun () -> Format:format(Registry) end),
  prometheus_summary:observe(Registry,
                             ?SCRAPE_SIZE,
                             [Registry, ContentType],
                             iolist_size(Scrape)),
  {ContentType, Scrape}.

encode_format(ContentType, Encoding, Scrape) ->
  Encoded = encode_format_(Encoding, Scrape),
  Registry = default,
  prometheus_summary:observe(Registry,
                             ?SCRAPE_ENCODED_SIZE,
                             [Registry, ContentType, Encoding],
                             iolist_size(Encoded)),
  {200, [{content_type, ContentType},
         {content_encoding, Encoding}], Encoded}.

encode_format_(<<"gzip">>, Scrape) ->
  zlib:gzip(Scrape);
encode_format_(<<"deflate">>, Scrape) ->
  ZStream = zlib:open(),
  zlib:deflateInit(ZStream),
  try
    zlib:deflate(ZStream, Scrape, finish)
  after
    zlib:deflateEnd(ZStream)
  end;
encode_format_(<<"identity">>, Scrape) ->
  Scrape.
