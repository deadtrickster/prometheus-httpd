-module(prometheus_http).

-export([reply/1,
         setup/0]).

-define(SCRAPE_DURATION, telemetry_scrape_duration_seconds).
-define(SCRAPE_SIZE, telemetry_scrape_size_bytes).
-define(SCRAPE_ENCODED_SIZE, telemetry_scrape_encoded_size_bytes).

%% @doc
%% Render metrics
%% @end
reply(#{path := Path,
        headers := Headers,
        registry := Registry,
        standalone := Standalone}) ->

  case prometheus_httpd_config:valid_path_and_registry(Path, Registry) of
    {true, RealRegistry} ->
      if_authorized(Path, Headers,
                    fun () ->
                        format_metrics(Headers, RealRegistry)
                    end);
    {registry_conflict, _ReqR, _ConfR} ->
      {409, [], <<>>};
    {registry_not_found, _ReqR} ->
      {404, [], <<>>};
    false ->
      maybe_render_index(Standalone, Path, Headers)
  end.

%% @doc
%% Initializes telemetry metrics.<br/>
%% *NOTE:* If you plug `prometheus_httpd' in your existing httpd instance,
%% you have to call this function manually.
%% @end
setup() ->
  TelemetryRegistry = prometheus_httpd_config:telemetry_registry(),

  ScrapeDuration = [{name, ?SCRAPE_DURATION},
                    {help, "Scrape duration"},
                    {labels, ["registry", "content_type"]},
                    {registry, TelemetryRegistry}],
  ScrapeSize = [{name, ?SCRAPE_SIZE},
                {help, "Scrape size, not encoded"},
                {labels, ["registry", "content_type"]},
                {registry, TelemetryRegistry}],
  ScrapeEncodedSize = [{name, ?SCRAPE_ENCODED_SIZE},
                       {help, "Scrape size, encoded"},
                       {labels, ["registry", "content_type", "encoding"]},
                       {registry, TelemetryRegistry}],

  prometheus_summary:declare(ScrapeDuration),
  prometheus_summary:declare(ScrapeSize),
  prometheus_summary:declare(ScrapeEncodedSize).

%% ===================================================================
%% Private Parts
%% ===================================================================

format_metrics(Headers, Registry) ->
  Accept = Headers("accept", "text/plain"),
  AcceptEncoding = Headers("accept-encoding", undefined),
  format_metrics(Accept, AcceptEncoding, Registry).

maybe_render_index(Standalone, Path, Headers) ->
  case Standalone of
    true ->
      MetricsPath = prometheus_httpd_config:path(),
      if_authorized(Path, Headers,
                    fun () ->
                        {200, [], prepare_index(MetricsPath)}
                    end);
    false ->
      false
  end.

if_authorized(URI, Headers, Fun) ->
  case prometheus_httpd_config:authorization() of
    {invalid_authorize, _} ->
      {500, [], <<>>};
    Auth ->
      case Auth(#{uri => URI,
                  headers => Headers}) of
        true ->
          Fun();
        false ->
          {403, [], <<>>}
      end
  end.

prepare_index(Path) ->
  FileName = filename:join([code:priv_dir(prometheus_httpd), "index.html"]),
  {ok, Content} = file:read_file(FileName),
  re:replace(Content, "M_E_T_R_I_C_S", Path, [global, {return, list}]).

format_metrics(Accept, AcceptEncoding, Registry) ->
  case negotiate_format(Accept) of
    undefined ->
      {406, [], <<>>};
    Format ->
      {ContentType, Scrape} = render_format(Format, Registry),
      case negotiate_encoding(AcceptEncoding) of
        undefined ->
          {406, [], <<>>};
        Encoding ->
          encode_format(ContentType, binary_to_list(Encoding), Scrape, Registry)
      end
  end.

negotiate_format(Accept) ->
  case prometheus_httpd_config:format() of
    auto ->
      Alternatives = prometheus_httpd_config:allowed_formats(),
      accept_header:negotiate(Accept, Alternatives);
    Format0 -> Format0
  end.

negotiate_encoding(AcceptEncoding) ->
  accept_encoding_header:negotiate(AcceptEncoding, [<<"gzip">>,
                                                    <<"deflate">>,
                                                    <<"identity">>]).

render_format(Format, Registry) ->
  ContentType = Format:content_type(),
  TelemetryRegistry = prometheus_httpd_config:telemetry_registry(),

  Scrape = prometheus_summary:observe_duration(
             TelemetryRegistry,
             ?SCRAPE_DURATION,
             [Registry, ContentType],
             fun () -> Format:format(Registry) end),
  prometheus_summary:observe(TelemetryRegistry,
                             ?SCRAPE_SIZE,
                             [Registry, ContentType],
                             iolist_size(Scrape)),
  {ContentType, Scrape}.

encode_format(ContentType, Encoding, Scrape, Registry) ->
  Encoded = encode_format_(Encoding, Scrape),
  TelemetryRegistry = prometheus_httpd_config:telemetry_registry(),

  prometheus_summary:observe(TelemetryRegistry,
                             ?SCRAPE_ENCODED_SIZE,
                             [Registry, ContentType, Encoding],
                             iolist_size(Encoded)),
  {200, [{content_type, binary_to_list(ContentType)},
         {content_encoding, Encoding}], Encoded}.

encode_format_("gzip", Scrape) ->
  zlib:gzip(Scrape);
encode_format_("deflate", Scrape) ->
  ZStream = zlib:open(),
  zlib:deflateInit(ZStream),
  try
    zlib:deflate(ZStream, Scrape, finish)
  after
    zlib:deflateEnd(ZStream)
  end;
encode_format_("identity", Scrape) ->
  Scrape.
