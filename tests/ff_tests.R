# ff tests that require being highly intentional about load/attach order, e.g. #3

library(bit)

if (isNamespaceLoaded("ff")) {
  cat("ff must be unloaded initially for this test to WAI\n")
  q("n")
}
chunk(1)

if (!requireNamespace("ff")) { # nolint: undesirable_function_linter. Good usage.
  cat("ff failed to load\n")
  q("n")
}
chunk(1)

if (!require("ff")) { # nolint: unused_import_linter. It is used, by chunk().
  cat("ff failed to attach\n")
  q("n")
}
chunk(1)
