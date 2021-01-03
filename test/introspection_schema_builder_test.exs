defmodule Artem.IntrospectionSchemaBuilderTest do
  use ExUnit.Case

  alias Artem.IntrospectionSchemaBuilder

  @ref %{
    module: Test,
    location: %{
      file: "test.json",
      line: 0
    }
  }

  test "builds schema definitions from introspection result" do
    json = File.read!("test/fixtures/test.json")

    {:ok, definitions} = IntrospectionSchemaBuilder.build_definitions(json, Test, @ref, [])

    {identifiers, [schema_declaration]} =
      definitions |> Enum.split_with(&Map.get(&1, :identifier))

    assert Enum.map(identifiers, & &1.identifier) == [
             :bar,
             :foo,
             :book,
             :category,
             :city,
             :comment,
             :complex_input,
             :cool_scalar,
             :human,
             :movie,
             :named,
             :post,
             :post_filter,
             :query,
             :titled,
             :user
           ]

    assert %Absinthe.Blueprint.Schema.SchemaDeclaration{} = schema_declaration
  end
end
