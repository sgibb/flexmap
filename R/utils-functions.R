#' create uppercase plural dimnames
#' @return character
#' @noRd
.Dimnames <- function() {
  dn <- .dimnames()
  paste0(toupper(substr(dn, 1L, 1L)), substr(dn, 2L, nchar(dn)), "s")
}
