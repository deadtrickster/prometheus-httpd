defmodule PrometheusHttpdCtTest do
  use Prometheus.Httpd.Case

  test "the truth" do
    assert 1 + 1 == 2
  end

  test "self test" do
    Prometheus.Httpd.Test.self_test(%{metrics_port: 8081,
                                      metrics_path: "metrics"})
    Prometheus.Httpd.Test.self_test([metrics_port: 8081, 
                                     metrics_path: "/metrics"])
    Prometheus.Httpd.Test.self_test(8081, 'metrics')
  end
end
