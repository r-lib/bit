# ff tests that require being highly intentional about load/attach order, e.g. #3

library(bit)

if (isNamespaceLoaded("ff")) {
  cat("ff must be unloaded initially for this test to WAI\n")
  q("n")
}
chunk(1)

if (!requireNamespace("ff")) {
  cat("ff failed to load\n")
  q("n")
}
chunk(1)

if (!require("ff")) {
  cat("ff failed to attach\n")
  q("n")
}
chunk(1)
