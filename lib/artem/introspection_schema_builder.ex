defmodule Artem.IntrospectionSchemaBuilder do
  @moduledoc false

  alias Absinthe.Blueprint
  alias Absinthe.Language

  @skip_builtin_types ~w(
      String
      Boolean
      Float
      Int
      ID
      __Type
      __Field
      __InputValue
      __EnumValue
      __DirectiveLocation
      __Directive
      __TypeKind
    )

  @skip_builtin_directives ~w(skip include)

  def build_definitions(json, module, ref, opts) do
    json_decoder = opts[:json] || Jason

    case json_decoder.decode(json) do
      {:ok, result} -> do_build_definitions(result, module, ref, opts)
      {:error, error} -> raise "Could not parse json #{error}"
    end
  end

  defp do_build_definitions(result, module, ref, opts) do
    if result["errors"] do
      raise """
      Something went wrong with introspection and it returned an error:

        #{List.first(result["errors"])["message"]}
      """
    end

    type_definitions =
      result["data"]["__schema"]["types"]
      |> Enum.reject(&(&1["name"] in @skip_builtin_types))
      |> Enum.map(&build_type_definition/1)

    directive_definitions =
      result["data"]["__schema"]["directives"]
      |> Enum.reject(&(&1["name"] in @skip_builtin_directives))
      |> Enum.map(&build_directive_definition/1)

    definitions = directive_definitions ++ type_definitions

    doc = %Blueprint{
      input: %Language.Document{definitions: definitions}
    }

    definitions =
      doc.input.definitions
      |> Enum.map(&Blueprint.Draft.convert(&1, doc))
      |> Enum.map(&put_ref(&1, ref, opts))
      |> Enum.map(fn type -> %{type | module: module} end)

    {:ok, definitions}
  end

  defp build_type_definition(%{"name" => "__Schema"} = type) do
    %Language.SchemaDeclaration{
      description: type["description"],
      directives: [],
      fields: build_field_definitions(type["fields"]),
      loc: %{column: nil, line: nil}
    }
  end

  defp build_type_definition(%{"kind" => "UNION"} = type) do
    %Language.UnionTypeDefinition{
      name: type["name"],
      description: type["description"],
      directives: build_directives(type),
      types: Enum.map(type["possibleTypes"], &build_type/1),
      loc: %{column: nil, line: nil}
    }
  end

  defp build_type_definition(%{"kind" => "OBJECT"} = type) do
    %Language.ObjectTypeDefinition{
      name: type["name"],
      description: type["description"],
      directives: build_directives(type),
      interfaces: Enum.map(type["interfaces"], &build_type/1),
      fields: build_field_definitions(type["fields"]),
      loc: %{line: nil, column: nil}
    }
  end

  defp build_type_definition(%{"kind" => "INPUT_OBJECT"} = type) do
    %Language.InputObjectTypeDefinition{
      name: type["name"],
      description: type["description"],
      directives: build_directives(type),
      fields: build_input_value_definitions(type["inputFields"]),
      loc: %{line: nil, column: nil},
      errors: []
    }
  end

  defp build_type_definition(%{"kind" => "ENUM"} = type) do
    %Language.EnumTypeDefinition{
      name: type["name"],
      description: type["description"],
      values: build_enum_value_definitions(type["enumValues"]),
      directives: build_directives(type),
      loc: %{line: nil, column: nil}
    }
  end

  defp build_type_definition(%{"kind" => "INTERFACE"} = type) do
    %Language.InterfaceTypeDefinition{
      name: type["name"],
      description: type["description"],
      fields: build_field_definitions(type["fields"]),
      directives: [],
      loc: %{line: nil, column: nil}
    }
  end

  defp build_type_definition(%{"kind" => "SCALAR"} = type) do
    %Language.ScalarTypeDefinition{
      name: type["name"],
      description: type["description"],
      directives: build_directives(type),
      loc: %{line: nil, column: nil}
    }
  end

  defp build_directive_definition(directive) do
    %Language.DirectiveDefinition{
      name: directive["name"],
      description: directive["description"],
      locations:
        Enum.map(directive["locations"], &(String.downcase(&1) |> String.to_existing_atom())),
      arguments: build_args(directive["args"]),
      loc: %{line: nil, column: nil}
    }
  end

  defp build_enum_value_definitions(enum_values) do
    for enum_value <- enum_values do
      %Language.EnumValueDefinition{
        value: enum_value["name"],
        description: enum_value["description"],
        directives: [],
        loc: %{line: nil, column: nil}
      }
    end
  end

  defp build_input_value_definitions(input_fields) do
    for input_field <- input_fields do
      %Language.InputValueDefinition{
        default_value: default_value(input_field["defaultValue"]),
        description: input_field["description"],
        directives: [],
        loc: %{column: nil, line: nil},
        name: input_field["name"],
        type: build_type(input_field["type"])
      }
    end
  end

  defp build_directives(directives) when is_list(directives) do
    for directive <- directives do
      %Language.Directive{
        arguments: build_args(directive["args"]),
        loc: %{column: nil, line: nil},
        name: directive["name"]
      }
    end
  end

  defp build_directives(%{"isDeprecated" => true} = type) do
    args =
      if type["deprecationReason"] do
        [
          %Language.Argument{
            loc: %{column: nil, line: nil},
            name: "reason",
            value: %Language.StringValue{
              loc: %{column: nil, line: nil},
              value: type["deprecationReason"]
            }
          }
        ]
      else
        []
      end

    [
      %Language.Directive{
        arguments: args,
        loc: %{column: nil, line: nil},
        name: "deprecated"
      }
    ]
  end

  defp build_directives(_type) do
    []
  end

  defp build_field_definitions(fields) do
    for field <- fields do
      %Language.FieldDefinition{
        name: field["name"],
        description: field["description"],
        arguments: build_args(field["args"]),
        directives: build_directives(field),
        type: build_type(field["type"]),
        complexity: nil,
        loc: %{line: nil, column: nil}
      }
    end
  end

  defp build_type(%{"ofType" => nil} = type) do
    %Language.NamedType{
      name: type["name"]
    }
  end

  defp build_type(%{"kind" => "NON_NULL"} = type) do
    %Language.NonNullType{
      loc: %{column: nil, line: nil},
      type: build_type(type["ofType"])
    }
  end

  defp build_type(%{"kind" => "LIST"} = type) do
    %Language.ListType{
      loc: %{column: nil, line: nil},
      type: build_type(type["ofType"])
    }
  end

  defp build_args(args) do
    for arg <- args do
      %Language.InputValueDefinition{
        default_value: default_value(arg["defaultValue"]),
        description: arg["description"],
        directives: [],
        loc: %{column: nil, line: nil},
        name: arg["name"],
        type: build_type(arg["type"])
      }
    end
  end

  def parse(default_value) do
    with {:ok, tokens} <- Absinthe.Lexer.tokenize(default_value),
         {:ok, default_value} <- :default_value_parser.parse(tokens) do
      default_value
    end
  end

  defp default_value(nil), do: nil
  defp default_value(default_value), do: parse(default_value)

  # remove put_ref, copied over from SDL schema's but serves no purpose here
  defp put_ref(%{fields: fields, directives: directives} = node, ref, opts) do
    %{
      node
      | fields: Enum.map(fields, &put_ref(&1, ref, opts)),
        directives: Enum.map(directives, &put_ref(&1, ref, opts))
    }
    |> do_put_ref(ref, opts)
  end

  defp put_ref(%{fields: fields} = node, ref, opts) do
    %{node | fields: Enum.map(fields, &put_ref(&1, ref, opts))}
    |> do_put_ref(ref, opts)
  end

  defp put_ref(%{arguments: args, directives: directives} = node, ref, opts) do
    %{
      node
      | arguments: Enum.map(args, &put_ref(&1, ref, opts)),
        directives: Enum.map(directives, &put_ref(&1, ref, opts))
    }
    |> do_put_ref(ref, opts)
  end

  defp put_ref(%{arguments: args} = node, ref, opts) do
    %{node | arguments: Enum.map(args, &put_ref(&1, ref, opts))}
    |> do_put_ref(ref, opts)
  end

  defp put_ref(%{directives: directives} = node, ref, opts) do
    %{node | directives: Enum.map(directives, &put_ref(&1, ref, opts))}
    |> do_put_ref(ref, opts)
  end

  defp put_ref(node, ref, opts), do: do_put_ref(node, ref, opts)

  defp do_put_ref(%{__reference__: nil} = node, ref, opts) do
    ref =
      case opts[:path] do
        nil ->
          ref

        path ->
          put_in(ref.location, %{
            file: {:unquote, [], [path]},
            line: node.source_location.line,
            column: node.source_location.column
          })
      end

    %{node | __reference__: ref}
  end

  defp do_put_ref(node, _ref, _opts), do: node
end
