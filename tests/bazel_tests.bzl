"""Rules for testing our own Bazel rules."""

"""Toolchains we're using for our test scripts."""

TOOLCHAINS = []

_bazelrc = """
startup --batch

build --verbose_failures
build --sandbox_debug
build --test_output=errors
build --spawn_strategy=standalone
build --genrule_strategy=standalone

test --test_strategy=standalone

build:isolate --fetch=False
"""

def _md5_sum_impl(ctx):
  out = ctx.new_file(ctx.label.name+".md5")
  ctx.actions.run_shell(
    inputs = ctx.files.srcs,
    outputs = [out],
    command = "md5sum %s > %s" % (' '.join([src.path for src in ctx.files.srcs]), out.path),
  )
  return struct(files=depset([out]))

md5_sum = rule(
    _md5_sum_impl,
    attrs = {
        "srcs": attr.label_list(allow_files = True),
    },
)

"""Output the MD5 sums of all the given files."""

def _bazel_test_script_impl(ctx):
  script_content = ''
  workspace_content = ''
  subdir = ""
  if ctx.attr.subdir:
    subdir = ctx.attr.subdir + "/"
  # Build the bazel startup args
  bazelrc = ctx.new_file(subdir + ".bazelrc")
  args = ["--bazelrc={0}".format(bazelrc.basename), "--nomaster_blazerc"]
  # Add the command and any command specific args
  args += [ctx.attr.command]
  if ctx.attr.config:
    args += ["--config", ctx.attr.config]
  for ext in ctx.attr.externals:
    root = ext.label.workspace_root
    _,_,ws = root.rpartition("/")
    workspace_content += 'local_repository(name = "{0}", path = "{1}/{2}")\n'.format(ws, ctx.attr._execroot.path, root)
  # finalise the workspace file
  if ctx.attr.workspace:
    workspace_content += ctx.attr.workspace
  workspace_file = ctx.new_file(subdir + "WORKSPACE")
  ctx.file_action(output=workspace_file, content=workspace_content)
  # finalise the script
  args += ctx.attr.args + [ctx.attr.target]
  # TODO(jml): Maybe factor this out into separate function? Try to use
  # '\n'.join() rather than appending lines quadratically.
  script_content += 'BASE=$(pwd)\n'
  script_content += 'cd {0}\n'.format(ctx.label.package)
  script_content += 'PACKAGE=$(pwd)\n'
  if ctx.attr.subdir:
    script_content += 'cd {0}\n'.format(ctx.attr.subdir)
    script_content += 'cp BUILD.in BUILD.bazel\n'
  script_content += 'WORKSPACE=$(pwd)\n'
  if ctx.attr.prepare:
    script_content += ctx.attr.prepare
  script_content += 'cd $WORKSPACE\n'
  script_content += 'echo {0} {1}\n'.format(ctx.attr._execroot.bazel, " ".join(args))
  script_content += '{0} {1}\n'.format(ctx.attr._execroot.bazel, " ".join(args))
  script_content += 'result=$?\n'
  if ctx.attr.check:
    script_content += ctx.attr.check
  script_content += "exit $result\n"
  script_file = ctx.new_file(ctx.label.name+".bash")
  ctx.file_action(output=script_file, executable=True, content=script_content)
  # finalise the bazel options
  ctx.file_action(output=bazelrc, content=_bazelrc)
  return struct(
    files = depset([script_file]),
    runfiles = ctx.runfiles([workspace_file, bazelrc])
  )

_bazel_test_script = rule(
    _bazel_test_script_impl,
    attrs = {
        "command": attr.string(
            mandatory = True,
            values = [
                "build",
                "test",
                "coverage",
                "run",
            ],
        ),
        "args": attr.string_list(default = []),
        "subdir": attr.string(),
        "target": attr.string(mandatory = True),
        "externals": attr.label_list(allow_files = True),
        "data": attr.label_list(allow_files = True),
        "workspace": attr.string(),
        "prepare": attr.string(),
        "check": attr.string(),
        "config": attr.string(default = "isolate"),
        "_execroot": attr.label(default = Label("@test_environment//:execroot")),
    },
    toolchains = TOOLCHAINS,
)

def bazel_test(name, command = None, args=None, subdir = None, target = None, tags=[], externals=[], data=[], workspace="", prepare="", check="", config=None):
  """Test a Bazel rule.

  Runs the `bazel` executable.

  Args:
    name: The name of the test
    command: The Bazel subcommand to run (e.g. 'run', 'build', 'test')
    args: A list of arguments to pass to Bazel
    subdir: ???
    target: ???
    tags: ???
    externals: ???
    data: ???
    workspace: Extra content to add to the WORKSPACE file used by tests.
    prepare: ???
    check: ???
    config: ???
  """
  script_name = name+"_script"

  _bazel_test_script(
      name = script_name,
      command = command,
      args = args,
      subdir = subdir,
      target = target,
      externals = externals,
      workspace = workspace,
      prepare = prepare,
      check = check,
      config = config,
  )
  native.sh_test(
      name = name,
      size = "large",
      timeout = "moderate",
      srcs = [script_name],
      tags = ["local", "bazel"] + tags,
      data = native.glob(["**/*"]) + externals + data,
  )

def _test_environment_impl(ctx):
  execroot, _, ws = str(ctx.path(".")).rpartition("/external/")
  bazel = ""
  if "BAZEL" in ctx.os.environ:
    bazel = ctx.os.environ["BAZEL"]
  elif "BAZEL_VERSION" in ctx.os.environ:
    home = ctx.os.environ["HOME"]
    bazel = home + "/.bazel/{0}/bin/bazel".format(ctx.os.environ["BAZEL_VERSION"])
  if bazel == "" or not ctx.path(bazel).exists:
    bazel = ctx.which("bazel")
  if ctx.name != ws:
    fail("workspace did not match, expected:", ctx.name, "got:", ws)
  ctx.file("WORKSPACE", """
workspace(name = "%s")
""" % ctx.name)
  ctx.file("BUILD", """
load("@io_jml_rules_haskell//tests:bazel_tests.bzl", "execroot")
execroot(
    name = "execroot",
    path = "{0}",
    bazel = "{1}",
    visibility = ["//visibility:public"],
)
""".format(execroot, bazel))

_test_environment = repository_rule(
    attrs = {},
    environ = [
        "BAZEL",
        "BAZEL_VERSION",
        "HOME",
    ],
    implementation = _test_environment_impl,
)

def test_environment():
  _test_environment(name="test_environment")

def _execroot_impl(ctx):
  return struct(
    path = ctx.attr.path,
    bazel = ctx.attr.bazel,
  )

execroot = rule(
    _execroot_impl,
    attrs = {
        "path": attr.string(mandatory = True),
        "bazel": attr.string(mandatory = True),
    },
)
