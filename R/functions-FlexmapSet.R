.showProcessingData <- function(object) {
  pd <- processingData(object)
  n <- length(pd)

  if (n) {
    if (n > 5L) {
      pd <- c(pd[1L], "...", pd[(n-3L):n])
    }
    cat("processingData:", paste0("  ", pd), sep="\n")
  } else {
    cat("processingData: none\n")
  }
}

.substractBackground <- function(object, id="Blank") {
  exprs(object) <- sweep(exprs(object),
                         .dim[c("feature", "dilution", "replicate")],
                         exprs(object)[,id,,, drop=FALSE],
                         "-")
  processingData(object) <- paste("Substract background", .pdim(exprs(object)))
  object
}

.filterBackground <- function(object, id="Blank") {
  if (is.character(id)) {
    id <- match(id, sampleNames(object))
  }
  object <- object[,-id,,]
  processingData(object) <- paste("Filter background", .pdim(exprs(object)))
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

  processingData(object) <- paste0("Average replicates (method='", method,
                                "') ", .pdim(exprs(object)))
  validObject(object)
  object
}

.normalise <- function(object, antitag) {
  stopifnot(dim(exprs(object))[.dim["feature"]] == length(antitag))
  exprs(object) <- exprs(object)/antitag
  processingData(object) <- paste("Normalise", .pdim(exprs(object)))
  object
}

#aggregateArray <- function(object, ...) {
#  sweep(exprs(object), .dim[c("sample", "feature", "dilution")],
#        mean, na.rm=TRUE)
#}

## technical replicates
#rowSums(exprs(object), dims=3)
