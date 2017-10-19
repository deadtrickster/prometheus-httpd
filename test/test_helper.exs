ExUnit.start()

defmodule Prometheus.Httpd.Case do
  defmacro __using__(_opts) do
    quote do

      use ExUnit.Case
      import ExUnit.CaptureIO

      setup do        
        {:ok, _} = :application.ensure_all_started(:prometheus_httpd)
        :prometheus_httpd.start()

        on_exit fn ->
          :application.set_env(:prometheus, :prometheus_http, [])
        end
      end

    end
  end
end
