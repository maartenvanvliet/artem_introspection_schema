defmodule ArtemIntrospectionSchemaTest do
  use ExUnit.Case

  defmodule StarWarsIntrospection do
    use Absinthe.Schema
    import Artem.ImportIntrospection

    import_introspection path: "test/fixtures/starwars.json"

    @pipeline_modifier __MODULE__

    def pipeline(pipeline) do
      pipeline
      |> Absinthe.Pipeline.without(Absinthe.Phase.Schema.Validation.InterfacesMustResolveTypes)
    end
  end

  test "starwars json" do
    {:ok, result} = Absinthe.Schema.introspect(StarWarsIntrospection)

    assert Jason.decode!(File.read!("test/fixtures/starwars.json"))["data"] == result[:data]
  end

  defmodule FileProvider do
    @behaviour Artem.SchemaProvider

    def get(opts) do
      body = File.read!(opts[:file])
      {:ok, body}
    end
  end

  defmodule TestIntrospection do
    use Absinthe.Schema
    import Artem.ImportIntrospection

    import_introspection provider: {FileProvider, file: "test/fixtures/test.json"}

    @pipeline_modifier __MODULE__

    def pipeline(pipeline) do
      pipeline
      |> Absinthe.Pipeline.without(Absinthe.Phase.Schema.Validation.InterfacesMustResolveTypes)
    end
  end
end
