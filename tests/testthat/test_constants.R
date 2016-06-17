context("constants")

test_that(".dim", {
  expect_identical(.dim, c(feature=1L, sample=2L, dilution=3L, replicate=4L))
  expect_equal(.dimnames(), c("feature", "sample", "dilution", "replicate"))
})
