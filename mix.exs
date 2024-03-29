defmodule Ejabberd.MixProject do
  use Mix.Project

  def project do
    [app: :ejabberd,
     version: "19.2.0",
     description: description(),
     elixir: "~> 1.8",
     elixirc_paths: ["lib"],
     compile_path: ".",
     compilers: [:asn1] ++ Mix.compilers,
     erlc_options: erlc_options(),
     erlc_paths: ["asn1", "src"],
     deps: deps(),
     package: package(),

     # Re-aliasing this to use the ct mix task.
     aliases: [test: "ct"],

     # Configuration for using this as an umbrella child app.
     config_path: get_config_path(),
     build_path: get_build_path(),
     deps_path: get_deps_path(),
     lockfile: get_lockfile(),

     # Coveralls specific configuration
     preferred_cli_env: [
       coveralls: :test, 
       "coveralls.json": :test
     ],
     test_coverage: [tool: ExCoveralls, test_task: "ct"],
     cover_enabled: true,
     cover_export_enabled: true]

  end

  def description do
    """
    Robust, ubiquitous and massively scalable Jabber / XMPP Instant Messaging platform.
    """
  end

  def application do
    [mod: {:ejabberd_app, []},
     applications: [:kernel, :stdlib, :sasl, :ssl],
     included_applications: [:lager, :logger, :mnesia, :inets, :p1_utils, :cache_tab,
                             :fast_tls, :stringprep, :fast_xml, :xmpp,
                             :stun, :fast_yaml, :esip, :jiffy, :p1_oauth2,
                             :base64url, :jose, :pkix, :os_mon]
     ++ cond_apps()]
  end

  defp if_function_exported(mod, fun, arity, okResult) do
    :code.ensure_loaded(mod)
    if :erlang.function_exported(mod, fun, arity) do
      okResult
    else
      []
    end
  end

  defp if_version_above(ver, okResult) do
    if :erlang.system_info(:otp_release) > ver do
      okResult
    else
      []
    end
  end

  defp erlc_options do
    # Use our own includes + includes from all dependencies
    includes = ["include"] ++ deps_include(["fast_xml", "xmpp", "p1_utils"])
    [:debug_info, {:d, :ELIXIR_ENABLED}] ++ cond_options() ++ Enum.map(includes, fn(path) -> {:i, path} end) ++
    if_version_above('20', [{:d, :DEPRECATED_GET_STACKTRACE}]) ++
    if_function_exported(:crypto, :strong_rand_bytes, 1, [{:d, :STRONG_RAND_BYTES}]) ++
    if_function_exported(:rand, :uniform, 1, [{:d, :RAND_UNIFORM}]) ++
    if_function_exported(:gb_sets, :iterator_from, 2, [{:d, :GB_SETS_ITERATOR_FROM}]) ++
    if_function_exported(:public_key, :short_name_hash, 1, [{:d, :SHORT_NAME_HASH}])
  end

  defp cond_options do
    for {:true, option} <- [{config(:graphics), {:d, :GRAPHICS}}], do:
    option
  end

  defp deps do
    [
      {:p1_utils, "1.0.24", override: true},
      {:cache_tab, "1.0.29", override: true},
      {:base64url, tag: "v1.0", git: "https://github.com/dvv/base64url.git", override: true},
      {:lager, "~> 3.6.0"},
      {:fast_xml, "~> 1.1"},
      {:xmpp, "~> 1.3.0"},
      {:stringprep, "~> 1.0"},
      {:fast_yaml, "~> 1.0"},
      {:fast_tls, "~> 1.1"},
      {:stun, "~> 1.0"},
      {:esip, "~> 1.0"},
      {:p1_mysql, "~> 1.0"},
      {:mqtree, "~> 1.0"},
      {:p1_pgsql, "~> 1.1"},
      {:jiffy, "~> 0.14.7"},
      {:p1_oauth2, "~> 0.6.1"},
      {:distillery, "~> 2.0"},
      {:pkix, "~> 1.0"},
      {:jose, "~> 1.8"},
      {:iconv, "1.0.13"},
      {:ezlib, "~> 1.0"},

      {:meck, "0.8.13", only: :test},
      {:excoveralls, "~> 0.11.0", only: :test},
      {:sqlite3, "~> 1.1", only: :test},
      {:riakc, "~> 2.4", only: :test},
      {:eredis, "~> 1.0", only: :test},
      {:epam, "~> 1.0", only: :test},
      {:luerl, "~> 0.3.1", only: :test},

      {:ex_doc, ">= 0.0.0", only: :dev},
    ]
  end

  defp deps_include(deps) do
    base = if Mix.Project.umbrella?() do
       "../../deps"
    else
      case Mix.Project.deps_paths()[:ejabberd] do
        nil -> "deps"
        _ -> ".."
      end
    end
    Enum.map(deps, fn dep -> base<>"/#{dep}/include" end)
  end

  defp cond_apps do
    for {:true, app} <- [{config(:redis), :eredis},
                         {config(:mysql), :p1_mysql},
                         {config(:pgsql), :p1_pgsql},
                         {config(:sqlite), :sqlite3},
                         {config(:zlib), :ezlib}], do:
      app
  end

  defp package do
    [# These are the default files included in the package
      files: ["lib", "src", "priv", "mix.exs", "include", "README.md", "COPYING"],
      maintainers: ["ProcessOne"],
      licenses: ["GPLv2"],
      links: %{"Site" => "https://www.ejabberd.im",
               "Documentation" => "http://docs.ejabberd.im",
               "Source" => "https://github.com/processone/ejabberd",
               "ProcessOne" => "http://www.process-one.net/"}]
  end

  defp vars do
    case :file.consult("vars.config") do
      {:ok,config} -> config
      _ -> [zlib: true]
    end
  end

  defp config(key) do
    case vars()[key] do
      nil -> false
      value -> value
    end
  end

  # Check if we're in a mix umbrella or not to check for our configuration
  def get_config_path do
    if Mix.Project.umbrella?() do
      "../../config/config.exs"
    else
      "config/config.exs"
    end
  end
  def get_build_path do
    if Mix.Project.umbrella?() do
      "../../_build"
    else
      "_build"
    end
  end
  def get_deps_path do
    if Mix.Project.umbrella?() do
      "../../deps"
    else
      "deps"
    end
  end
  def get_lockfile do
    if Mix.Project.umbrella?() do
      "../../mix.lock"
    else
      "mix.lock"
    end
  end
end

defmodule Mix.Tasks.Compile.Asn1 do
  use Mix.Task
  alias Mix.Compilers.Erlang

  @recursive true
  @manifest ".compile.asn1"

  def run(args) do
    {opts, _, _} = OptionParser.parse(args, switches: [force: :boolean])

    project      = Mix.Project.config
    source_paths = project[:asn1_paths] || ["asn1"]
    dest_paths    = project[:asn1_target] || ["src"]
    mappings     = Enum.zip(source_paths, dest_paths)
    options      = project[:asn1_options] || []

    force = case opts[:force] do
        :true -> [force: true]
        _ -> [force: false]
    end

    Erlang.compile(manifest(), mappings, :asn1, :erl, force, fn
      input, output ->
        options = options ++ [:noobj, outdir: Erlang.to_erl_file(Path.dirname(output))]
        case :asn1ct.compile(Erlang.to_erl_file(input), options) do
          :ok -> {:ok, :done}
          error -> error
        end
    end)
  end

  def manifests, do: [manifest()]
  defp manifest, do: Path.join(Mix.Project.manifest_path, @manifest)

  def clean, do: Erlang.clean(manifest())
end
