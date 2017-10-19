defmodule Prometheus.Httpd.Test do
  def self_test(%{metrics_port: metrics_port, metrics_path: metrics_path}) do
    self_test(metrics_port, metrics_path)
  end
  def self_test(options) when is_list(options) do
    self_test(Keyword.fetch!(options, :metrics_port), Keyword.fetch!(options, :metrics_path))
  end

  def self_test(metrics_port, metrics_path) do
    :prometheus_httpd_ct.self_test(metrics_port, metrics_path)
  end
end
