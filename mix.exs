defmodule PrometheusHTTPD.Mixfile do
  use Mix.Project

  def project do
    [app: :prometheus_httpd,
     version: "1.1.0",
     description: description(),
     package: package()]
  end

  defp description do
    """
    Prometheus.io inets httpd exporter
    """
  end

  defp package do
    [build_tools: ["rebar3"],
     maintainers: ["Ilya Khaprov"],
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/deadtrickster/prometheus_httpd",
              "Prometheus.erl" => "https://hex.pm/packages/prometheus",
              "Prometheus.ex" => "https://hex.pm/packages/prometheus_ex",
              "Dashboards" => "https://github.com/deadtrickster/beam-dashboards",
              "Ecto Instrumenter" => "https://hex.pm/packages/prometheus_ecto",
              "Phoenix Instrumenter" => "https://hex.pm/packages/prometheus_phoenix",
              "Plugs Instrumenter/Exporter" => "https://hex.pm/packages/prometheus_plugs",
              "Process info Collector" => "https://hex.pm/packages/prometheus_process_collector"},
     files: ["priv", "src", "README.md", "rebar.config"]]
  end
end
