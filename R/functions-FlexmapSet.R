.showProcessData <- function(object) {
  pd <- processData(object)
  n <- length(pd)

  if (n) {
    if (n > 5L) {
      pd <- c(pd[1L], "...", pd[(n-3L):n])
    }
    cat("processData:", paste0("  ", pd), sep="\n")
  } else {
    cat("processData: none\n")
  }
}

.substractBackground <- function(object, id="Blank") {
  exprs(object) <- sweep(exprs(object),
                         .dim[c("feature", "dilution", "replicate")],
                         exprs(object)[,id,,, drop=FALSE],
                         "-")
  processData(object) <- paste("Substract background", .pdim(exprs(object)))
  object
}

.filterBackground <- function(object, id="Blank") {
  if (is.character(id)) {
    id <- match(id, sampleNames(object))
  }
  object <- object[,-id,,]
  processData(object) <- paste("Filter background", .pdim(exprs(object)))
  object
}

.averageReplicates <- function(object, method="mean") {
  ex <- exprs(object)
  ex <- array(rowMeans(ex, dims=.dim["replicate"] - 1L, na.rm=TRUE),
                       dim=c(dim(ex)[.dim[c("feature", "sample", "dilution")]],
                             1L))
  object <- object[,,,1L, drop=FALSE]
  dimnames(ex) <- dimnames(object)
  exprs(object) <- ex

  processData(object) <- paste0("Average replicates (method='", method,
                                "') ", .pdim(exprs(object)))
  validObject(object)
  object
}

.normalise <- function(object, antitag) {
  stopifnot(dim(exprs(object))[.dim["feature"]] == length(antitag))
  exprs(object) <- exprs(object)/antitag
  processData(object) <- paste("Normalise", .pdim(exprs(object)))
  object
}

#aggregateArray <- function(object, ...) {
#  sweep(exprs(object), .dim[c("sample", "feature", "dilution")],
#        mean, na.rm=TRUE)
#}

## technical replicates
#rowSums(exprs(object), dims=3)
