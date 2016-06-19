context("utils")

test_that(".Dimnames", {
  expect_equal(.Dimnames(),
               c("Features", "Samples", "Dilutions", "Replicates"))
})

test_that(".ndim", {
  expect_equal(.ndim(matrix(1, nrow=2, ncol=5)), 2)
  expect_equal(.ndim(array(1, dim=1:4)), 4)
})

test_that(".pdim", {
  expect_identical(.pdim(matrix(1, nrow=2, ncol=5)), "[2,5]")
  expect_identical(.pdim(array(1, dim=1:4)), "[1,2,3,4]")
})
