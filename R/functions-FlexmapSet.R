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

.sweep <- function(object,
                   MARGIN=.dim[c("feature", "dilution", "replicate")],
                   STATS, FUN, processing="", ...) {
  exprs(object) <- sweep(exprs(object), MARGIN=MARGIN, STATS=STATS,
                         FUN=FUN, ...)
  processingData(object) <- paste(processing, .pdim(exprs(object)))
  object
}

.substractBackground <- function(object, id="Blank") {
  .sweep(object, STATS=exprs(object)[,id,,, drop=FALSE], FUN="-",
         processing="Substract background")
}

.normalise <- function(object, antitag) {
  .sweep(object, STATS=antitag, FUN="/", processing="Normalise")
}

.filterBackground <- function(object, id="Blank") {
  if (is.character(id)) {
    id <- match(id, sampleNames(object))
  }
  object <- object[,-id,,]
  processingData(object) <- paste("Filter background", .pdim(exprs(object)))
  object
}

.calcSD <- function(object, na.rm=TRUE) {
  sqrt(apply(exprs(object), .dim[c("feature", "sample", "dilution")], var,
             na.rm=na.rm))
}

.calcCV <- function(object, na.rm=TRUE) {
  .calcSD(object, na.rm=na.rm) /
    rowMeans(exprs(object), dims=.dim["replicate"] - 1L, na.rm=TRUE)
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
