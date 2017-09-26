load("@io_jml_rules_haskell//haskell:def.bzl", "hs_library")

hs_library(
    name = "foo",
    srcs = ["Foo.hs"],
    deps = [":bar"],
    visibility = ["//visibility:public"],
)

hs_library(
    name = "bar",
    srcs = ["Bar.hs"],
)
