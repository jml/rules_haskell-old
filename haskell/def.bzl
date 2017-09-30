"""Haskell Rules

These build rules are used for building Haskell projects with Bazel.

In comments and docstrings, the first person refers to jml, aka Jonathan Lange.
"""

# TODO:
# - (maybe) change _hs_compile to create output directory
# - allow passing compiler options
#   - profiling / debug builds
#   - language flags, warnings, etc.
#   - threaded
#   - -j4 (i.e. concurrent builds)
# - some sort of equivalent of exposed-modules / other-modules?
# - consider whether to set 'main-is' by default when only one source file
#
# - data dependencies (probably requires simulating cabal) (#3)
# - set up toolchain / repository rules for GHC so we don't have to assume it's installed (#4)
# - library with C dependencies (e.g. PCRE)
# - tool for generating BUILD files from cabal files (#5)
# - generate skylark documentation (#7)


"""Valid Haskell source files."""
HASKELL_FILETYPE = [
    "hs",
    "lhs",
]

# TODO: Once Bazel 0.6.0 is released, specify allowed fields using 'fields'
# parameter.
ghc_output = provider()

def _haskell_toolchain(ctx):
  """Everything we need to build Haskell with GHC."""
  # TODO: Assemble this from something like 'repositories', which fetches the
  # toolchain and uses that to build things, rather than assuming a system GHC
  # is installed.
  return struct(
    ghc_path = "ghc",
  )

def _dirname(path_str):
  if '/' in path_str:
    return path_str[:path_str.rfind('/')]
  return ''

def _get_output_dir(ctx):
  return '/'.join([ctx.bin_dir.path, _dirname(ctx.build_file_path)])

def _path_segments(path):
  return [s for s in path.split('/') if s not in ('.', '')]

def _declare_output_file(actions, build_dir, src_dir, src_file, extension):
  """Declare the output file of a GHC process.

  :param actions: The ``ctx.actions`` object
  :param build_dir: The directory BUILD is in.
  :param src_dir: The root of the module hierarchy
  :param src_file: File within 'src_dir' that's being compiled.
      e.g. ``$src_dir/Module/Hierarchy/Name.hs``
  :param extension: The extension of the new file, either ``'.o'`` or ``'.hi'``.
  :return: A 'File' pointing at ``Module/Hierarchy/Name.(o|hi)``
  """
  src_segments = _path_segments(src_file.path)
  module_root = _path_segments(build_dir) + _path_segments(src_dir)
  if src_segments[:len(module_root)] != module_root:
    fail("Expected source file, %s, to be underneath source directory, %s" % (src_file, src_dir))
  module_segments = src_segments[len(module_root):]
  if len(module_segments) == 0:
    fail("No source file left after trimming source directory (src_file=%s, src_dir=%s)" % (src_file, src_dir))
  basename = module_segments[-1]
  extension_index = basename.rfind('.')
  if extension_index == -1:
    fail("Somehow got unexpected source filename, %s. Must be one of %s" % (basename, HASKELL_FILETYPE))
  new_basename = basename[:extension_index] + extension
  new_path = '/'.join(module_segments[:-1] + [new_basename])
  return actions.declare_file(new_path)

def _hs_compile(toolchain, name, actions, srcs, deps, build_dir, output_dir, main_file=None, src_dir=None):
  """Compile a single Haskell module.

  To be able to use this, a reverse dependency is going to have to either

    1. explicitly add the right directory to its search path with '-i', or
    2. explicitly include the *.o (and maybe the *.hi?) in its args

  Still trying to figure out how to express "the right directory" in Bazel
  language. It's a directory such that the path to the *.o file looks like the
  Haskell module name. e.g. for module Foo.Bar.Baz, the directory, $dir, has
  to be something where $dir/Foo/Bar/Baz.{o,hi} exist.

  Not sure if I need to create a directory and use that as the -odir & -hidir
  for GHC, or whether I need to calculate the base directory that we're using,
  or whether Bazel already has that calculated for me.

  I haven't actually tried invoking ghc with *.o files in the args directly.
  Or maybe I have but I've forgotten the results.

  """
  object_files = []
  interface_files = []
  for src in srcs:
    if src == main_file:
      object_files.append(actions.declare_file("Main.o"))
      interface_files.append(actions.declare_file("Main.hi"))
    else:
      object_files.append(_declare_output_file(actions, build_dir, src_dir, src, '.o'))
      interface_files.append(_declare_output_file(actions, build_dir, src_dir, src, '.hi'))
  output_files = object_files + interface_files

  import_directories = []

  immediate_hs_objects = depset([])
  transitive_hs_objects = depset([])
  transitive_hs_interfaces = depset([])
  for dep in deps:
    # XXX: We get duplicate directories. Should probably de-dupe them.
    import_directories.append('-i%s' % dep[ghc_output].import_directory)
    immediate_hs_objects += dep[ghc_output].hs_objects
    transitive_hs_objects += dep[ghc_output].transitive_hs_objects
    transitive_hs_interfaces += dep[ghc_output].transitive_hs_interfaces

  #print('srcs = %s, deps = %s, dirs = %s --> %s' % (srcs, dep_files, import_directories, output_files))

  ghc_args = [
    '-c',  # So we just compile things, no linking
    '-i',  # Empty the import directory list
    ] + import_directories + [
      '-odir', output_dir,
      '-hidir', output_dir,
    ] + [src.path for src in srcs]
  # XXX: stack also includes
  # -ddump-hi
  # -ddump-to-file
  # -optP-include
  # -optP.stack-work/.../cabal_macros.h
  #
  # - various output dir controls
  # - various package db controls
  #
  # Also what about...
  # - optimizations
  # - warnings
  # - concurrent builds (-j4)
  # - -threaded (I guess only relevant for executables)
  actions.run(
    inputs = srcs + (immediate_hs_objects + transitive_hs_interfaces).to_list(),
    outputs = output_files,
    executable = toolchain.ghc_path,
    arguments = ghc_args,
    progress_message = ("Compiling Haskell modules %s" % (srcs,)),
    mnemonic = 'HsCompile',
    # TODO: Figure out how we can do without this.
    use_default_shell_env = True,
  )
  return ghc_output(
    files = depset(output_files),
    hs_objects = depset(object_files),
    hs_interfaces = depset(interface_files),
    transitive_hs_objects = transitive_hs_objects + depset(object_files),
    transitive_hs_interfaces = transitive_hs_interfaces + depset(interface_files),
    # XXX: Would really like to have a better answer than this.
    import_directory = output_dir,
  )

def _hs_module_impl(ctx):
  """A single Haskell module.

  At the moment this only works with a single file in srcs.

  Assumes that every source file is named after the module name that it
  contains (with dots replaced by directory separators). For example, the
  module Data.Person would be in the file Data/Person.hs on Unix/Linux/Mac, or
  Data\Person.hs on Windows.

  See https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using.html#getting-started-compiling-programs
  """
  toolchain = _haskell_toolchain(ctx)
  return _hs_compile(
    toolchain, ctx.label.name, ctx.actions, ctx.files.srcs, ctx.attr.deps,
    _dirname(ctx.build_file_path), _get_output_dir(ctx), src_dir=ctx.attr.src_dir)

def _hs_binary_impl(ctx):
  """A Haskell executable."""
  toolchain = _haskell_toolchain(ctx)
  lib_self = _hs_compile(
    toolchain, ctx.label.name, ctx.actions, ctx.files.srcs, ctx.attr.deps,
    _dirname(ctx.build_file_path), _get_output_dir(ctx),
    main_file=ctx.file.main_is, src_dir=ctx.attr.src_dir)
  # XXX: I guess we have to use ghc to link executables.
  ctx.actions.run(
      inputs = lib_self.transitive_hs_objects + ctx.files.data,
      outputs = [ctx.outputs.executable],
      executable = toolchain.ghc_path,
      arguments = [
        "-o", ctx.outputs.executable.path,
      ] + [obj.path for obj in lib_self.transitive_hs_objects],
      use_default_shell_env = True,
  )

def _hs_test_impl(ctx):
  return _hs_binary_impl(ctx)

_hs_attrs = {
    "srcs": attr.label_list(
        allow_files = HASKELL_FILETYPE,
    ),
    "deps": attr.label_list(
        allow_files = False,
    ),
    "data": attr.label_list(
        allow_files = True,
    ),
    "src_dir": attr.string(
        doc = 'The root of the module hierarchy',
    ),
}

_hs_binary_attrs = {
    "main_is": attr.label(
      allow_single_file = HASKELL_FILETYPE,
    ),
}

hs_library = rule(
    attrs = _hs_attrs,
    implementation = _hs_module_impl,
)

hs_binary = rule(
    attrs = dict(_hs_attrs.items() + _hs_binary_attrs.items()),
    executable = True,
    implementation = _hs_binary_impl,
)

hs_test = rule(
    attrs = dict(_hs_attrs.items() + _hs_binary_attrs.items()),
    executable = True,
    implementation = _hs_test_impl,
    test = True,
)
