setMethod("initialize", "FlexmapSet",
          function(.Object, assayData, phenoData, featureData, dilutionData,
                   replicateData, exprs=array(double(), dim=c(0,0,0,0)), ... ) {
  if (missing(assayData) && missing(exprs)) {
    stop("provide at most one of 'assayData' or 'exprs' to initialize FlexmapSet",
         call.=FALSE)
  }
  if (!missing(assayData)) {
    if (is.null(assayData[["exprs"]])) {
      stop("if 'assayData' is given it has to contain an 'exprs' array")
    }
    exprs <- assayData[["exprs"]]
  }

  if (missing(phenoData)) {
    phenoData <- .annotatedDataFrameFromArray(exprs, dimname="sample")
  }
  if (missing(featureData)) {
    featureData <- .annotatedDataFrameFromArray(exprs, dimname="feature")
  }
  if (missing(dilutionData)) {
    dilutionData <- .annotatedDataFrameFromArray(exprs, dimname="dilution")
  }
  if (missing(replicateData)) {
    replicateData <- .annotatedDataFrameFromArray(exprs, dimname="replicate")
  }

  if (!missing(assayData)) {
    callNextMethod(.Object, assayData=assayData, phenoData=phenoData,
                   featureData=featureData, dilutionData=dilutionData,
                   replicateData=replicateData, ...)
  } else {
    callNextMethod(.Object, phenoData=phenoData, featureData=featureData,
                   dilutionData=dilutionData, replicateData = replicateData,
                   exprs=exprs, ...)
  }
})

setValidity("FlexmapSet", function(object) {
  msg <- NULL

  if (!isCurrent(object)[["FlexmapSet"]]) {
    msg <- validMsg(msg, "out-of-date class version 'FlexmapSet'")
  }
  dims <- dims(object)

  if (ncol(dims) > 0L) {
    ## dilutionData
    if (dims[.dim["dilution"], 1L] != dim(dilutionData(object))[[1L]]) {
      msg <- validMsg(msg,
        "dilution numbers differ between assayData and dilutionData")
    }
    if (!identical(dilutionNames(object), rownames(dilutionData(object)))) {
      msg <- validMsg(msg,
        "dilutionNames differ between assayData and dilutionData")
    }
    ## replicateData
    if (dims[.dim["replicate"], 1L] != dim(replicateData(object))[[1L]]) {
      msg <- validMsg(msg,
        "replicate numbers differ between assayData and replicateData")
    }
    if (!identical(replicateNames(object), rownames(replicateData(object)))) {
      msg <- validMsg(msg,
        "replicateNames differ between assayData and replicateData")
    }
  }
  if (is.null(msg)) TRUE else msg
})

setMethod(FlexmapSet, "missing",
  function(assayData,
  phenoData=AnnotatedDataFrame(),
  featureData=AnnotatedDataFrame(),
  dilutionData=AnnotatedDataFrame(),
  replicateData=AnnotatedDataFrame(),
  experimentData=MIAME(),
  annotation=character(),
  protocolData=AnnotatedDataFrame(), ...) {
  .FlexmapSet(assayData=assayDataNew(exprs=array(double(),dim=c(0,0,0,0))),
              assayData, phenoData=phenoData, featureData=featureData,
              dilutionData=dilutionData, replicateData=replicateData,
              experimentData=experimentData, annotation=annotation,
              protocolData=protocolData, ...)
})

setMethod(FlexmapSet, "environment",
 function(assayData,
           phenoData=annotatedDataFrameFrom(assayData, dimname="sample"),
           featureData=annotatedDataFrameFrom(assayData, dimname="feature"),
           dilutionData=annotatedDataFrameFrom(assayData, dimname="dilution"),
           replicateData=annotatedDataFrameFrom(assayData, dimname="replicate"),
           experimentData=MIAME(),
           annotation=character(),
           protocolData=annotatedDataFrameFrom(assayData, dimname="sample"),
           ...) {
  .FlexmapSet(assayData, phenoData=phenoData, featureData=featureData,
              dilutionData=dilutionData, replicateData=replicateData,
              experimentData=experimentData, annotation=annotation,
              protocolData=protocolData, ...)
})

setMethod(FlexmapSet, "array",
  function(assayData,
           phenoData=annotatedDataFrameFrom(assayData, dimname="sample"),
           featureData=annotatedDataFrameFrom(assayData, dimname="feature"),
           dilutionData=annotatedDataFrameFrom(assayData, dimname="dilution"),
           replicateData=annotatedDataFrameFrom(assayData, dimname="replicate"),
           experimentData=MIAME(),
           annotation=character(),
           protocolData=annotatedDataFrameFrom(assayData, dimname="sample"),
           ...) {
  .FlexmapSet(exprs=assayData, phenoData=phenoData, featureData=featureData,
              dilutionData=dilutionData, replicateData=replicateData,
              experimentData=experimentData, annotation=annotation,
              protocolData=protocolData, ...)
})

setMethod("[", "FlexmapSet",
          function(x, i, j, k, l, ..., drop=FALSE) {
  if (!isVersioned(x) || !isCurrent(x)["FlexmapSet"]) {
    x <- updateObject(x)
  }

  if (!missing(i)) {
    featureData(x) <- featureData(x)[i,, ..., drop=drop]
  }

  if (!missing(j)) {
    phenoData(x) <- phenoData(x)[j,, ..., drop=drop]
    protocolData(x) <- protocolData(x)[j,, ..., drop=drop]
  }

  if (!missing(k)) {
    dilutionData(x) <- dilutionData(x)[k,, ..., drop=drop]
  }

  if (!missing(l)) {
    replicateData(x) <- replicateData(x)[l,, ..., drop=drop]
  }

  orig <- assayData(x)
  mode <- storageMode(x)
  assayData(x) <-
    switch(mode,
           environment =,
           lockedEnvironment = {
             ad <- new.env(parent=emptyenv())
             for (nm in ls(orig)) ad[[nm]] <- orig[[nm]][i, j, k, l, ..., drop=drop]
             if (mode == "lockedEnvironment") lockEnvironment(ad, TRUE)
             ad
           },
           list = {
             lapply(orig, function(obj) obj[[nm]][i, j, k, l, ..., drop=drop])
           })
  processData(x) <- paste("Subset", .pdim(exprs(x)))
  validObject(x)
  x
})

setMethod("dilutionData", "FlexmapSet", function(object) object@dilutionData)

setReplaceMethod("dilutionData",
                 signature=signature(object="FlexmapSet",
                                     value="AnnotatedDataFrame"),
                 function(object, value) {
  object@dilutionData <- value
  object
})

setMethod("dilutionNames", "FlexmapSet", function(object) {
  if (dim(object)[.dim["dilution"]]) {
    dimnames(object)[[.dim["dilution"]]]
  } else {
    character(0L)
  }
})

setMethod("dim", signature(x="FlexmapSet"),
          function(x) {
  d <- callNextMethod(x)
  names(d) <- .Dimnames()
  d
})

setMethod("dims", signature(object="FlexmapSet"),
          function(object) {
  d <- callNextMethod(object)
  rownames(d) <- .Dimnames()
  d
})

setMethod("dims", signature(object="FlexmapSet"),
          function(object) {
  d <- callNextMethod(object)
  rownames(d) <- .Dimnames()
  d
})

setMethod("dimnames", signature(x="FlexmapSet"),
          function(x) {
  dimnames(assayDataElement(x, "exprs"))
})

setMethod("exprs", signature(object="FlexmapSet"),
          function(object) assayDataElement(object, "exprs"))

setReplaceMethod("exprs", signature(object="FlexmapSet", value="array"),
                 function(object, value) {
  assayDataElementReplace(object, "exprs", value)
})

setMethod("replicateData", "FlexmapSet", function(object) object@replicateData)

setReplaceMethod("replicateData",
                 signature=signature(object="FlexmapSet",
                                     value="AnnotatedDataFrame"),
                 function(object, value) {
  object@replicateData <- value
  object
})

setMethod("replicateNames", "FlexmapSet", function(object) {
  if (dim(object)[.dim["replicate"]]) {
    dimnames(object)[[.dim["replicate"]]]
  } else {
    character(0L)
  }
})

setMethod("processData", "FlexmapSet", function(object) object@processData)

setReplaceMethod("processData", "FlexmapSet",
                 function(object, value, append=TRUE) {
  if (append) {
    object@processData <- c(object@processData, value)
  } else {
    object@protocolData <- value
  }
  object
})

setMethod("show", "FlexmapSet", function(object) {
  cat(class(object), " (storageMode: ", storageMode(object), ")\n", sep="")

  adim <- dim(object)

  if (length(adim) > 1L) {
    cat("assayData:", paste(adim, tolower(names(adim)), collapse=", "), "\n")
    cat("  element names:", paste(assayDataElementNames(object),
                                  collapse=", "), "\n")
    cat(.showAnnotatedDataFrame(protocolData(object), "protocolData"),
        sep="\n")
    cat(.showAnnotatedDataFrame(phenoData(object), "phenoData"),
        sep="\n")
    cat(.showAnnotatedDataFrame(featureData(object), "featureData"),
        sep="\n")
    cat(.showAnnotatedDataFrame(dilutionData(object), "dilutionData"),
        sep="\n")
    cat(.showAnnotatedDataFrame(replicateData(object), "replicateData"),
        sep="\n")
    cat("experimentData: use 'experimentData(object)'\n")

    .showProcessData(object)
  }
})
