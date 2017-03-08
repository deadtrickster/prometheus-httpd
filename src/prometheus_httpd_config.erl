-module(prometheus_httpd_config).

-export([path/0,
         format/0,
         allowed_formats/0,
         port/0]).

%% Macros.
-define(DEFAULT_PATH, <<"/metrics">>).
-define(DEFAULT_FORMAT, auto).
-define(DEFAULT_PORT, 8081).

-define(DEFAULT_CONFIG, [{path, ?DEFAULT_PATH},
                         {format, ?DEFAULT_FORMAT},
                         {port, ?DEFAULT_PORT}]).

%%%===================================================================
%%% API
%%%===================================================================

path() -> get_value(path, ?DEFAULT_PATH).

format() -> get_value(format, ?DEFAULT_FORMAT).

allowed_formats() ->
  [{prometheus_text_format:content_type(), prometheus_text_format},
   {prometheus_protobuf_format:content_type(), prometheus_protobuf_format}].

port() -> get_value(port, ?DEFAULT_FORMAT).

%%%===================================================================
%%% Private functions
%%%===================================================================

get_value(Key, Default) -> proplists:get_value(Key, config(), Default).

config() -> application:get_env(prometheus, prometheus_httpd, ?DEFAULT_CONFIG).
