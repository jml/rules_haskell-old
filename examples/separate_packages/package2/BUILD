load("@io_jml_rules_haskell//haskell:def.bzl", "hs_library")

hs_library(
    name = "baz",
    srcs = ["Baz.hs"],
    deps = ["//examples/separate_packages/package1:foo"],
    visibility = ["//visibility:public"],
)
