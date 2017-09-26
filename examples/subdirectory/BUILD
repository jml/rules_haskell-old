load("@io_jml_rules_haskell//haskell:def.bzl", "hs_binary", "hs_library")

hs_library(
    name = "foo",
    srcs = ["src/Foo.hs"],
    deps = [":bar"]
)

hs_library(
    name = "bar",
    srcs = ["src/Bar.hs"],
)

hs_binary(
    name = "subdirectory",
    srcs = ["cmd/Main.hs"],
    deps = [
        ":foo",
        ":bar",
    ],
)
