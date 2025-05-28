defmodule Termbox2.MixProject do
  use Mix.Project

  def project do
    [
      app: :termbox2_elixir,
      version: "2.0.0",
      description: "Elixir wrapper for the termbox2_nif NIF library, providing terminal UI capabilities via termbox2.",
      licenses: ["MIT"],
      links: %{ 
        "GitHub" => "https://github.com/Hydepwns/termbox2-nif",
        "Docs" => "https://hexdocs.pm/termbox2_elixir"
      },
      elixir: "~> 1.16",
      start_permanent: Mix.env() == :prod,
      deps: deps()
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
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
      {:termbox2_nif, "~> 0.1.9"},
      {:mox, ">= 1.0.0", only: :test},
      {:stream_data, ">= 0.5.0", only: :test},
      {:ex_doc, ">= 0.0.0", only: :dev, runtime: false}
    ]
  end
end
