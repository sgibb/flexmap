context("constants")

test_that(".dim", {
  expect_identical(.dim, c(sample=1L, feature=2L, dilution=3L, replicate=4L))
  expect_equal(.dimnames(), c("sample", "feature", "dilution", "replicate"))
})
