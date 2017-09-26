load("@io_jml_rules_haskell//haskell:def.bzl", "hs_binary", "hs_library")

hs_library(
    name = "module_one_foo",
    srcs = ["src/ModuleOne/Foo.hs"],
    deps = [":module_one_bar"],
    src_dir = "src",
)

hs_library(
    name = "module_one_bar",
    srcs = ["src/ModuleOne/Bar.hs"],
    src_dir = "src",
)

hs_library(
    name = "module_two_foo",
    srcs = ["src/ModuleTwo/Foo.hs"],
    deps = [":module_two_bar"],
    src_dir = "src",
)

hs_library(
    name = "module_two_bar",
    srcs = ["src/ModuleTwo/Bar.hs"],
    src_dir = "src",
)

hs_binary(
    name = "subdirectory",
    srcs = ["cmd/Main.hs"],
    deps = [
        ":module_one_foo",
        ":module_two_foo",
    ],
    src_dir = "cmd",
)
