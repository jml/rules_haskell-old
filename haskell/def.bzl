"""Haskell Rules

These build rules are used for building Haskell projects with Bazel.

In comments and docstrings, the first person refers to jml, aka Jonathan Lange.
"""

# TODO:
# - transitive dependencies
# - BUILD file not at module root (e.g. BUILD, src/Foo/Bar.hs)
# - depending on things in other packages
# - hs_test rule
# - change _hs_compile to take multiple srcs
# - (maybe) change _hs_compile to create output directory
# - understand rest of Stack-provided GHC options
# - data dependencies (probably requires simulating cabal)
# - set up toolchain / repository rules for GHC so we don't have to assume it's installed
# - allow passing compiler options
#   - profiling / debug builds
#   - language flags, warnings, etc.
#   - threaded
#   - -j4 (i.e. concurrent builds)
# - library with C dependencies (e.g. PCRE)
# - tool for generating BUILD files from cabal files
# - tool for generating BUILD files from hpack files
# - generate skylark documentation


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
  return path_str[:path_str.rfind('/')]

def _get_output_dir(ctx, _input):
  return '/'.join([ctx.bin_dir.path, _dirname(ctx.build_file_path)])

def _hs_compile(toolchain, name, actions, src, deps, output_dir):
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
  # TODO: I think we can easily change this to compile multiple files.
  if src.extension not in HASKELL_FILETYPE:
    # XXX: We probably want to allow srcs that aren't Haskell files (genrule
    # results? *.o files?). For now, keeping it simple.
    fail("Can only build Haskell libraries from source files: %s" % (src.path,))

  object_file = actions.declare_file(_change_extension(src, 'o'))
  interface_file = actions.declare_file(_change_extension(src, 'hi'))

  import_directories = []
  dep_files = depset([])
  for dep in deps:
    import_directories.append('-i%s' % dep[ghc_output].import_directory)
    dep_files += dep[ghc_output].files

  ghc_args = [
    '-c',  # So we just compile things, no linking
    '-i',  # Empty the import directory list
    ] + import_directories + [
    # Dodgy hack that jml doesn't understand to make the already-compiled
    # dependencies visible to GHC (although *which* dependencies?).
    #'-i%s' % ctx.configuration.bin_dir.path,  # <-- not entirely correct
    '-o',  object_file.path,
    '-ohi', interface_file.path,
    src.path,
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
  ]
  actions.run(
    inputs = [src] + dep_files.to_list(),
    outputs = [object_file, interface_file],
    executable = toolchain.ghc_path,
    arguments = ghc_args,
    progress_message = ("Compiling Haskell module %s" % (src,)),
    mnemonic = 'HsCompile',
    # TODO: Figure out how we can do without this.
    use_default_shell_env = True,
  )
  return ghc_output(
    files = depset([object_file, interface_file]),
    hs_object = object_file,
    hs_interface = interface_file,
    # XXX:Would really like to have a better answer than this.
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
  if len(ctx.files.srcs) > 1:
    fail("We only support building modules from one source file: %s" % (ctx.files.srcs))
  [src] = ctx.files.srcs
  return _hs_compile(
    toolchain, ctx.label.name, ctx.actions, src, ctx.attr.deps,
    _get_output_dir(ctx, src))

def _hs_binary_impl(ctx):
  """A Haskell executable."""
  toolchain = _haskell_toolchain(ctx)
  if len(ctx.files.srcs) > 1:
    fail("We only support building binaries from one source file: %s" % (ctx.files.srcs))
  [src] = ctx.files.srcs
  lib_self = _hs_compile(
    toolchain, ctx.label.name, ctx.actions, src, ctx.attr.deps,
    _get_output_dir(ctx, src))
  objects = [x[ghc_output].hs_object for x in ctx.attr.deps] + [lib_self.hs_object]
  # XXX: I guess we have to use ghc to link executables.
  ctx.actions.run(
      inputs = objects + ctx.files.data,
      outputs = [ctx.outputs.executable],
      executable = toolchain.ghc_path,
      arguments = [
        "-o", ctx.outputs.executable.path,
      ] + [obj.path for obj in objects],
      use_default_shell_env = True,
  )

def _change_extension(file_object, new_extension):
   """Return the basename of 'file_object' with a new extension.

   e.g.

       _change_extension(file_obj, 'o')

   Will change the extension from '.c' or '.hs' to '.o'.
   """
   return file_object.basename[:-len(file_object.extension)] + new_extension

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
}

hs_library = rule(
    attrs = _hs_attrs,
    implementation = _hs_module_impl,
)

hs_binary = rule(
    attrs = _hs_attrs,
    executable = True,
    implementation = _hs_binary_impl,
)
