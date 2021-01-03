defmodule Artem.SchemaProvider do
  @moduledoc """
  Behaviour for getting the introspection schema

  Example for getting it over http

  ```elixir
  defmodule HttpProvider do
    @behaviour Artem.SchemaProvider

    def get(opts) do
      introspection =
        [:code.priv_dir(:absinthe), "graphql", "introspection.graphql"]
        |> Path.join()
        |> File.read!()

      payload =
        %{
          "operationName" => "IntrospectionQuery",
          "query" => introspection,
          "variables" => %{}
        }
        |> Jason.encode!()
        |> String.to_charlist()

      url = Keyword.get(opts, :url) |> String.to_charlist()

      {:ok, {_, _, body}} =
        :httpc.request(
          :post,
          {url, [], 'application/json', payload},
          [],
          []
        )

      {:ok, body}
    end
  end
  ```

  See `Artem.ImportIntrospection.import_introspection/2` on how to use a schema provider
  """
  @callback get(opts :: keyword()) :: {:ok, String.t()} | {:error, String.t()}
end
