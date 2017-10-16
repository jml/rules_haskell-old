load("@io_jml_rules_haskell//haskell:def.bzl", "hs_binary")

hs_binary(
    name = "cabal-to-bazel",
    srcs = ["cmd/Main.hs"],
    src_dir = "cmd",
    packages = [
        "Cabal",
        "directory",
        "filepath",
        "ghc",
        "ghc-paths",
    ],
)

