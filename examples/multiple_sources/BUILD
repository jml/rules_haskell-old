load("@io_jml_rules_haskell//haskell:def.bzl", "hs_binary", "hs_library")

hs_binary(
    name = "greeter",
    srcs = glob(["cmd/*.hs"]),
    deps = [":greeter_lib"],
    src_dir = 'cmd',
)

hs_library(
    name = "greeter_lib",
    srcs = glob(["src/**/*.hs"]),
    src_dir = 'src',
)
