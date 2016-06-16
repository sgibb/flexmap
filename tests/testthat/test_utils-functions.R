context("utils-functions")

test_that(".annotatedDataFrameFromArray", {
  a <- array(NA, dim=c(5, 4, 3, 2),
             dimnames=list(paste0("A", 1:5), paste0("B", 1:4),
                           paste0("C", 1:3), paste0("D", 1:2)))

  expect_error(.annotatedDataFrameFromArray(a),
               "argument \"dimname\" is missing, with no default")
  expect_error(.annotatedDataFrameFromArray(a, dimname="foo"),
               "'arg' should be one of")

  s <- .annotatedDataFrameFromArray(a, dimname="sample")
  f <- .annotatedDataFrameFromArray(a, dimname="feature")
  d <- .annotatedDataFrameFromArray(a, dimname="dilution")
  r <- .annotatedDataFrameFromArray(a, dimname="replicate")
  n <- .annotatedDataFrameFromArray(NULL, dimname="sample")

  expect_is(s, "AnnotatedDataFrame")
  expect_is(f, "AnnotatedDataFrame")
  expect_is(d, "AnnotatedDataFrame")
  expect_is(r, "AnnotatedDataFrame")
  expect_is(n, "AnnotatedDataFrame")

  expect_equal(dim(s), c(sampleNames=5, sampleColumns=0))
  expect_equal(dim(f), c(featureNames=4, featureColumns=0))
  expect_equal(dim(d), c(dilutionNames=3, dilutionColumns=0))
  expect_equal(dim(r), c(replicateNames=2, replicateColumns=0))
  expect_equal(dim(n), c(sampleNames=0, sampleColumns=0))
})
