#' create uppercase plural dimnames
#' @return character
#' @noRd
.Dimnames <- function() {
  dn <- .dimnames()
  paste0(toupper(substr(dn, 1L, 1L)), substr(dn, 2L, nchar(dn)), "s")
}

#' number of dimensions
#' @param x matrix/array
#' @return numieric
#' @noRd
.ndim <- function(x) {
  length(dim(x))
}

#' print/catable dims
#' @param x array
#' @return character
.pdim <- function(x) {
  paste0("[", paste0(dim(x), collapse=","), "]")
}
