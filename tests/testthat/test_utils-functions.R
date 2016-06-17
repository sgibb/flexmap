context("utils")

test_that(".Dimnames", {
  expect_equal(.Dimnames(),
               c("Features", "Samples", "Dilutions", "Replicates"))
})
