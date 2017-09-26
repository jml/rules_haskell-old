load("@io_jml_rules_haskell//haskell:def.bzl", "hs_binary")
load("@io_jml_rules_haskell//tests:bazel_tests.bzl", "bazel_test")

bazel_test(
    name = "single_binary",
    command = "build",
    data = [
        "//tests:rules_haskell_deps",
    ],
    externals = [
        "@io_jml_rules_haskell//:README.md",
        "@local_config_cc//:cc_wrapper",
    ],
    target = "//:hello_world",
)

hs_binary(
    name = "hello_world",
    srcs = ["HelloWorld.hs"],
    main_is = "HelloWorld.hs",
)
