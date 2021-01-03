defmodule Artem.ImportIntrospection do
  @external_resource "./README.md"
  @moduledoc """
  #{File.read!(@external_resource) |> String.split("---", parts: 2) |> List.last()}
  """

  alias Absinthe.Schema

  @type import_introspection_option :: {:path, String.t() | Macro.t()}
  @doc """
  Import types defined using the results of the introspection query.

  ### Examples

  ```
  import_introspection path: "/path/to/introspection.json"

  import_introspection provider: {FileProvider, file: "test/fixtures/test.json"}
  ```
  """
  defmacro import_introspection(opts) when is_list(opts) do
    __CALLER__
    |> do_import_introspection(nil, opts)
  end

  @doc """
  See `import_introspection/1`
  """
  @spec import_introspection(String.t() | Macro.t(), [import_introspection_option()]) :: Macro.t()
  defmacro import_introspection(introspection, opts \\ []) do
    __CALLER__
    |> do_import_introspection(introspection, opts)
  end

  defp do_import_introspection(env, nil, opts) do
    case Keyword.fetch(opts, :path) do
      {:ok, path} ->
        [
          quote do
            @__absinthe_import_introspection_path__ unquote(path)
          end,
          do_import_introspection(
            env,
            quote do
              File.read!(@__absinthe_import_introspection_path__)
            end,
            opts
          ),
          quote do
            @external_resource @__absinthe_import_introspection_path__
          end
        ]

      :error ->
        case Keyword.fetch(opts, :provider) do
          {:ok, {provider, opts}} ->
            [
              do_import_introspection(
                env,
                quote do
                  case unquote(provider).get(unquote(opts)) do
                    {:ok, body} -> body
                    {:error, error} -> raise Schema.Notation.Error, error
                  end
                end,
                opts
              )
            ]

          :error ->
            raise Schema.Notation.Error,
                  "Must provide `:path` option to `import_introspection` unless passing a raw json string as the first argument"
        end
    end
  end

  defp do_import_introspection(env, json, opts) do
    ref = Schema.Notation.build_reference(env)

    quote do
      case Artem.IntrospectionSchemaBuilder.build_definitions(
             unquote(json),
             __MODULE__,
             unquote(Macro.escape(ref)),
             unquote(Macro.escape(opts))
           ) do
        {:ok, definitions} ->
          @__absinthe_sdl_definitions__ definitions ++
                                          (Module.get_attribute(
                                             __MODULE__,
                                             :__absinthe_sdl_definitions__
                                           ) || [])

        {:error, error} ->
          raise Absinthe.Schema.Notation.Error,
                "`import_introspection` could not parse JSON:\n#{error}"
      end
    end
  end
end
