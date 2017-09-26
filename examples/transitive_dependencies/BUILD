load("@io_jml_rules_haskell//haskell:def.bzl", "hs_binary", "hs_library")

hs_library(
    name = "foo",
    srcs = ["Foo.hs"],
    deps = [":bar"],
)

hs_library(
    name = "bar",
    srcs = ["Bar.hs"],
    deps = [":baz"],
)

hs_library(
    name = "baz",
    srcs = ["Baz.hs"],
)

hs_binary(
    name = "transitive_dependencies",
    srcs = ["Main.hs"],
    deps = [
        ":foo",
    ],
)
