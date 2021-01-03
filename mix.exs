defmodule ArtemIntrospectionSchema.MixProject do
  use Mix.Project

  @url "https://github.com/maartenvanvliet/artem_introspection_schema"

  def project do
    [
      app: :artem_introspection_schema,
      version: "0.1.0",
      elixir: "~> 1.11",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      name: "Artem Introspection Schema",
      description:
        "Artem Introspection Schema is a library to build Absinthe schema's from introspection query results",
      package: [
        maintainers: ["Maarten van Vliet"],
        licenses: ["MIT"],
        links: %{"GitHub" => @url},
        files: ~w(LICENSE README.md lib mix.exs)
      ],
      docs: [
        main: "Artem.ImportIntrospection",
        canonical: "http://hexdocs.pm/artem_introspection_schema",
        source_url: @url
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:absinthe, "~> 1.5"},
      {:jason, "~> 1.1", only: [:dev, :test]},
      {:ex_doc, "~> 0.22", only: [:dev, :test]},
      {:credo, "~> 1.4", only: [:dev, :test], runtime: false}
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
    ]
  end
end
