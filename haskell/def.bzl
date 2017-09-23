"""Haskell Rules

These build rules are used for building Haskell projects with Bazel.
"""

"""Valid Haskell source files."""
HASKELL_FILETYPE = ["hs", "lhs"]


def _haskell_toolchain(ctx):
  """Everything we need to build Haskell with GHC."""
  # TODO: Assemble this from something like 'repositories', which fetches the
  # toolchain and uses that to build things, rather than assuming a system GHC
  # is installed.
  return struct(
    ghc_path = "ghc",
  )

def _hs_compile(toolchain, name, actions, src):
  """Compile a single Haskell module."""
  if src.extension not in HASKELL_FILETYPE:
    # XXX: We probably want to allow srcs that aren't Haskell files (genrule
    # results? *.o files?). For now, keeping it simple.
    fail("Can only build Haskell libraries from source files: %s" % (src.path,))

  object_file = actions.declare_file('%s.o' % name, sibling=src)
  interface_file = actions.declare_file('%s.hi' % name, sibling=src)

  ghc_args = [
    '-c',  # So we just compile things, no linking
    '-i',  # Empty the import directory list
    # Dodgy hack that jml doesn't understand to make the already-compiled
    # dependencies visible to GHC (although *which* dependencies?).
    #'-i%s' % ctx.configuration.bin_dir.path,  # <-- not entirely correct
    '-o',  object_file.path,
    '-ohi', interface_file.path,
    src.path,
    # XXX: stack also includes
    # -ddump-hi
    # -ddump-to-file
    # -fbuilding-cabal-package -- this just changes some warning text
    # -static  -- use static libraries, if possibly
    # -dynamic-too
    # -optP-include
    # -optP.stack-work/.../cabal_macros.h
    # -this-unit-id <label-name>-<version>-<thigummy>
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
    inputs = [src],
    outputs = [object_file, interface_file],
    executable = toolchain.ghc_path,
    arguments = ghc_args,
    progress_message = ("Compiling Haskell module %s" % (src,)),
    mnemonic = 'HsCompile',
    # TODO: Figure out how we can do without this.
    use_default_shell_env = True,
  )
  return struct(
    hs_object = object_file,
    hs_interface = interface_file,
  )


def _hs_module_impl(ctx):
  """A single Haskell module.

  At the moment this only works with a single file in srcs.
  """
  toolchain = _haskell_toolchain(ctx)
  if len(ctx.files.srcs) > 1:
    fail("We only support building modules from one source file: %s" % (ctx.files.srcs))
  [src] = ctx.files.srcs
  return _hs_compile(toolchain, ctx.label.name, ctx.actions, src)

def _hs_binary_impl(ctx):
  """A Haskell executable."""
  toolchain = _haskell_toolchain(ctx)
  if len(ctx.files.srcs) > 1:
    fail("We only support building binaries from one source file: %s" % (ctx.files.srcs))
  [src] = ctx.files.srcs
  lib_self = _hs_compile(toolchain, ctx.label.name, ctx.actions, src)
  objects = [x.hs_object for x in ctx.attr.deps] + [lib_self.hs_object]
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
   """Return the basename of 'file_object' with a new extension."""
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
