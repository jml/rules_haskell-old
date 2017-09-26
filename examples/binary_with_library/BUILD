load("@io_jml_rules_haskell//haskell:def.bzl", "hs_binary", "hs_library")

hs_binary(
    name = "greeter",
    srcs = ["Main.hs"],
    deps = [":greetings"],
)

hs_library(
    name = "greetings",
    srcs = ["Greetings.hs"],
)
