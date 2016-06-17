#' create an AnnotatedDataFrame
#' @param object array with dimnames
#' @param dimname name of dimension to use for rownames etc.
#' @return AnnotatedDataFrame
#' @noRd
.annotatedDataFrameFromArray <- function(object, dimname, ...) {

  i <- .dim[match.arg(dimname, .dimnames())]

  if (!is.null(object) && length(i)) {
    n <- dim(object)[i]
    rnm <- dimnames(object)[[i]]

    data <- data.frame(numeric(n), row.names=rnm)[, FALSE]
  } else {
    data <- data.frame()
  }

  dimLabels <- paste0(dimname, c("Names", "Columns"))

  AnnotatedDataFrame(data=data, dimLabels=dimLabels)
}

setMethod("annotatedDataFrameFrom",
          signature(object="array"), function(object, byrow, ...) {
  .annotatedDataFrameFromArray(object, ...)
})

#' modified output of show,AnnotatedDataFrame
#' @param object AnnotatedDataFrame
#' @param header character, headline instead of "An object of class ADF"
#' @return character
#' @noRd
.showAnnotatedDataFrame <- function(object, header) {
  stopifnot(is(object, "AnnotatedDataFrame"))

  out <- capture.output(object)

  if (!missing(header)) {
    out <- gsub("An object of class 'AnnotatedDataFrame'", header, out)
  }
  out <- out[!grepl("varMetadata:", out)]
  out
}
