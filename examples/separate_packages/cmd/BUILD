load("@io_jml_rules_haskell//haskell:def.bzl", "hs_binary")

hs_binary(
    name = "cmd",
    srcs = ["Main.hs"],
    deps = [
        "//examples/separate_packages/package1:foo",
        "//examples/separate_packages/package2:baz",
    ],
)
