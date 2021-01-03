# Artem.IntrospectionSchema

## [![Hex pm](http://img.shields.io/hexpm/v/artem_introspection_schema.svg?style=flat)](https://hex.pm/packages/artem_introspection_schema) [![Hex Docs](https://img.shields.io/badge/hex-docs-9768d1.svg)](https://hexdocs.pm/artem_introspection_schema) [![License](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)![.github/workflows/elixir.yml](https://github.com/maartenvanvliet/artem_introspection_schema/workflows/.github/workflows/elixir.yml/badge.svg)

---

Library to create Absinthe schema's from introspection queries.

This can be used to e.g. create Graphql clients which verify
outgoing graphql documents against external schema's created
from introspection. This can provide e.g. compile time safety of
those documents, so you can be sure that the graphql documents you
write will validate against the external schema.

It leverages the same import mechanism as the SDL schema's in Absinthe.

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `artem_introspection_schema` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:artem_introspection_schema, "~> 0.1.0"}
  ]
end
```

## Usage

Given the results of an introspection query you can create an Absinthe schema as follows:

```elixir
  defmodule TestIntrospection do
    use Absinthe.Schema
    import Artem.ImportIntrospection

    import_introspection path: "test/fixtures/test.json"

    @pipeline_modifier __MODULE__

    def pipeline(pipeline) do
      pipeline
      |> Absinthe.Pipeline.without(Absinthe.Phase.Schema.Validation.InterfacesMustResolveTypes)
    end
  end
```

Note that a pipeline modifier is applied to not validate interfaces. Since this is an external schema we don't have any resolvers or ways to resolve interfaces, so this phase can be skipped. Depending on the external schema there may be other (validation) phases that have to be skipped because e.g. they use an older or newer version of the grapql spec and Absinthe doesn't validate them accordingly.

### Schema providers

You can also use a schema provider to retrieve the schema. This is a flexible way if you want to load the introspection results more dynamically, e.g. over http.

See `Artem.SchemaProvider` for an example and `Artem.ImportIntrospection.import_introspection/2`

## Documentation

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at [https://hexdocs.pm/artem_introspection_schema](https://hexdocs.pm/artem_introspection_schema).
