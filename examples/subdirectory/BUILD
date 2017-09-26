load("@io_jml_rules_haskell//haskell:def.bzl", "hs_binary", "hs_library")

hs_library(
    name = "foo",
    srcs = ["src/Foo.hs"],
    deps = [":bar"],
    src_dir = 'src',
)

hs_library(
    name = "bar",
    srcs = ["src/Bar.hs"],
    src_dir = 'src',
)

hs_binary(
    name = "subdirectory",
    srcs = ["cmd/Main.hs"],
    deps = [
        ":foo",
        ":bar",
    ],
    src_dir = 'cmd',
)
