context("methods-AnnotatedDataFrame")

test_that("annotatedDataFrameFrom", {
  a <- array(NA, dim=c(5, 4, 3, 2),
             dimnames=list(paste0("A", 1:5), paste0("B", 1:4),
                           paste0("C", 1:3), paste0("D", 1:2)))

  expect_error(annotatedDataFrameFrom(a),
               "argument \"dimname\" is missing, with no default")
  expect_error(annotatedDataFrameFrom(a, dimname="foo"),
               "'arg' should be one of")

  f <- annotatedDataFrameFrom(a, dimname="feature")
  s <- annotatedDataFrameFrom(a, dimname="sample")
  d <- annotatedDataFrameFrom(a, dimname="dilution")
  r <- annotatedDataFrameFrom(a, dimname="replicate")
  ## should never happen
  n <- .annotatedDataFrameFromArray(NULL, dimname="sample")

  expect_is(f, "AnnotatedDataFrame")
  expect_is(s, "AnnotatedDataFrame")
  expect_is(d, "AnnotatedDataFrame")
  expect_is(r, "AnnotatedDataFrame")
  expect_is(n, "AnnotatedDataFrame")

  expect_equal(dim(f), c(featureNames=5, featureColumns=0))
  expect_equal(dim(s), c(sampleNames=4, sampleColumns=0))
  expect_equal(dim(d), c(dilutionNames=3, dilutionColumns=0))
  expect_equal(dim(r), c(replicateNames=2, replicateColumns=0))
  expect_equal(dim(n), c(sampleNames=0, sampleColumns=0))

  expect_equal(rownames(f), paste0("A", 1:5))
  expect_equal(rownames(s), paste0("B", 1:4))
  expect_equal(rownames(d), paste0("C", 1:3))
  expect_equal(rownames(r), paste0("D", 1:2))
  expect_equal(rownames(n), character())
})

test_that(".showAnnotatedDataFrame", {
  expect_error(.showAnnotatedDataFrame(data.frame()))

  d <- AnnotatedDataFrame(data.frame(foo=1:10, bar=1:10))
  expect_equal(.showAnnotatedDataFrame(d), capture.output(d)[-4])
  expect_equal(.showAnnotatedDataFrame(d, "foodata"),
               c("foodata", capture.output(d)[-c(1, 4)]))
  expect_equal(.showAnnotatedDataFrame(AnnotatedDataFrame(), "foodata"),
               "foodata: none")
})
