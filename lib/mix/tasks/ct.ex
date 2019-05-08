defmodule Mix.Tasks.Ct do
  use Mix.Task

  @moduledoc """
  Module to run erlang's ct framework from a mix task.

  Options available:
  --dir
    * Directory that has the location of the tests.
    * The cwd will be appended to the file passed.
    * Defaults to "<cwd>/test"
  --logdir
    * Directory where the logs should be stored.
    * The cwd will be appended to the file passed.
    * Defaults to "<cwd>/logs"
  --suite
    * Test suite to run.
    * The cwd will be appended to the file passed.
    * Defaults to "<cwd>/test/ejabberd_SUITE.erl"
  --ct_hooks
    * Hooks requested
    * Defaults to "cth_surefire"
  --cover
    * Where the cover.spec file is for coverage configuration.
    * The cwd will be appended to the file passed.
    * Defaults to "<cwd>/cover.spec"
  --include
    * Files to include.  MUST BE A LIST OF COMMA SEPERATED FILES.
    * The cwd will be appended to each of the files.
    * Example: "include/test1.hrl,./include/test2.hrl"
    * Defaults to "include/,tools,deps/p1_utils/include,deps/fast_xml/include,deps/xmpp/include"
  --name
    * Name that this test run should be.
    * Defaults to "ct_test_run_<current_utc_timestamp>_<current_hostname>"
  """

  @logs_to_keep 5
  @default_includes [
    "include/",
    "tools",
    "deps/p1_utils/include",
    "deps/fast_xml/include",
    "deps/xmpp/include"
  ]

  def run(args) do
    strict_options = [
      dir: :string,
      logdir: :string,
      suite: :string,
      ct_hooks: :string,
      cover: :string,
      include: :string,
      name: :string
    ]
    {options, _, _} = OptionParser.parse(args, strict: strict_options)

    Mix.env :test

    # Get current fallback test run name.
    {:ok, datetime} = DateTime.now("Etc/UTC")
    datetime_string = DateTime.to_string(datetime)
    {:ok, hostname} = :inet.gethostname
    default_label   = "ct_test_run_#{datetime_string}_#{hostname}"

    cwd       = "#{File.cwd!}/apps/ejabberd"
    dir       = Keyword.get(options, :dir, "#{cwd}/test")                       |> String.to_charlist
    logdir    = Keyword.get(options, :logdir, "#{cwd}/logs")                    |> String.to_charlist
    suite     = Keyword.get(options, :suite, "#{cwd}/test/ejabberd_SUITE.erl")  |> String.to_charlist
    ct_hooks  = Keyword.get(options, :ct_hooks, "cth_surefire")                 |> String.to_atom
    cover     = Keyword.get(options, :cover, "#{cwd}/cover.spec")               |> String.to_charlist
    label     = Keyword.get(options, :name, default_label)                      |> String.to_charlist
    include   = Keyword.get(options, :include, @default_includes |> Enum.join(","))

    # Create the logdir.
    File.mkdir(logdir)

    # Convert all includes to charlists
    chars_include = 
      include
      |> String.split(",")
      |> Enum.map(fn x -> 
        "#{cwd}/#{x}" |> String.to_charlist 
      end)

    result = :ct.run_test([
      {:dir, dir},
      {:logdir, logdir},
      {:suite, suite},
      {:ct_hooks, ct_hooks},
      {:cover, cover},
      {:include, chars_include},
      {:label, label},
      {:keep_logs, @logs_to_keep}
    ]) 

    case result do
      {:error, _} ->
        Mix.raise("Tests errored upon start")
        :erlang.halt(1)
      {:ok, failed, {user_skipped, _auto_skipped}} ->
        Mix.raise("Test failures detected")
        :erlang.halt(if (failed + user_skipped) == 0, do: 0, else: 1)
      _ ->
        :erlang.halt(0)
    end
  end
end
