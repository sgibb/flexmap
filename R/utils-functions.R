#' create an AnnotatedDataFrame
#' @param object array with dimnames
#' @param dimname name of dimension to use for rownames etc.
#' @return AnnotatedDataFrame
#' @noRd
.annotatedDataFrameFromArray <- function(object, dimname, ...) {

  choices <- c("sample", "feature", "dilution", "replicate")
  dimname <- match.arg(dimname, choices)
  i <- which(dimname == choices)

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
