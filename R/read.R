#' Read FlexmapSet
#' @param exprsFile character, file path to expression data in csv.
#' @param phenoDataFile character, file path to phenotype data in csv.
#' @param experimentDataFile character, file path to experiment data file in
#' MIAME format.
#' @param phenoDataArgs list, arguments to be passed to underlying functions.
#' @param epxerimentDataArgs list, arguments to be passed to underlying
#' functions.
#' @param sep field separator character, see also
#' \code{\link[utils]{read.table}}.
#' @param \ldots further arguments passed to the constructor of FlexmapSet.
#' @return \code{\linkS4class{FlexmapSet}}
#' @export
readFlexmapSet <- function(exprsFile, phenoDataFile, experimentDataFile,
                           phenoDataArgs=list(sep=sep, ...),
                           experimentDataArgs=list(...),
                           notesFile,
                           sep=";", ...) {
  ## exprs
  if (missing(exprsFile)) {
    stop("Expression can not be missing!")
  }
  if (!file.exists(exprsFile)) {
    stop("File ", sQuote(exprsFile), " doesn't exists!")
  }

  exprs <- .readFlexmapCsv(exprsFile, sep=sep)

  if (!missing(phenoDataFile)) {
    if (!file.exists(phenoDataFile)) {
      stop("File ", sQuote(phenoDataFile), " doesn't exists!")
    }
    phenoDataArgs$file <- phenoDataFile
    pd <- do.call(.readPhenoDataCsv, phenoDataArgs)

    i <- match(dimnames(exprs)[[.dim["sample"]]], sampleNames(pd))

    if (anyNA(i)) {
      warning("The phenoData file contains some ID's ",
              "that are not present in the expression file.")
    }
    pd <- pd[i,]
  } else {
    pd <- .annotatedDataFrameFromArray(exprs, "sample")
  }

  if (!missing(experimentDataFile)) {
    experimentDataArgs$file <- experimentDataFile
    if (!file.exists(experimentDataFile)) {
      stop("File ", sQuote(experimentDataFile), " doesn't exists!")
    }
    ed <- do.call(read.MIAME, experimentDataArgs)
  } else {
    ed <- MIAME()
  }

  if (!missing(notesFile)) {
    notes(ed) <- readLines(notesFile)
  }

  obj <- FlexmapSet(assayData=exprs, phenoData=pd, experimentData=ed,
                    processData=paste("Read FLEXMAP data", .pdim(exprs)))
  validObject(obj)
  obj
}

#' Read flexmap csv file format
#'
#' In general it is: Sample; (Replicate); (Dilution); Antigen1/Feature1; ...;
#' FeatureN
#' @param file file path/connection
#' @param sep field separator
#' @return array, dimensions = features x samples x dilution x replicates
#' (because eSet requires features x samples instead of samples x features)
#' @noRd
.readFlexmapCsv <- function(file, sep=";") {
  header <- scan(file, what=character(), sep=sep, nlines=1L, quiet=TRUE)
  content <- scan(file, what=c(character(1L),
                               replicate(length(header) - 1L, double())),
                  skip=as.numeric(is.character(file)),
                  sep=sep, quiet=TRUE, multi.line=FALSE)

  i <- match(.dimnames(), tolower(header), nomatch=0L)
  names(i) <- .dimnames()

  ## samples
  sa <- content[[i["sample"]]]
  sl <- sort.int(unique(sa))
  sn <- length(sl)

  ## features
  fl <- unlist(header[-i])
  fn <- length(fl)

  ## dilutions
  if (i["dilution"]) {
    da <- unlist(content[i["dilution"]])
    dl <- sort.int(unique(da))
  } else {
    da <- rep.int("1", length(sa))
    dl <- "1"
  }
  dn <- length(dl)

  ## replicate
  if (i["dilution"]) {
    rr <- sort.int(paste0(content[[i["sample"]]], content[[i["dilution"]]]),
                   index.return=TRUE)
  } else {
    rr <- sort.int(content[[i["sample"]]], index.return=TRUE)
  }
  rrl <- rle(rr$x)$lengths
  ra <- sequence(rrl)[rr$ix]
  rl <- 1L:max(rrl)
  rn <- length(rl)

  ## find correct order (starting at the deepest level)
  o <- order(ra, da, sa)
  o <- rep(0L:(fn - 1L), each=length(o)) * length(o) + o

  aperm(array(unlist(content[-i])[o],
              dim=c(sn, dn, rn, fn), dimnames=list(sl, dl, rl, fl)),
        perm=c(4L, 1L, 2L, 3L))
}

#' Read phenoData csv file
#'
#' Has to have a first column containing sample ids.
#'
#' @param file file path/connection
#' @param header first line == header, see ?read.table
#' @param sep field sep
#' @param stringsAsFactors stringsAsFactors, replace default with FALSE
#' @param ... furhter arguments to read.table
#' @return AnnotatedDataFrame
#' @noRd
.readPhenoDataCsv <- function(file, header=TRUE, sep=";",
                              stringsAsFactors=FALSE, ...) {
  d <- read.table(file, header=header, sep=sep,
                  stringsAsFactors=stringsAsFactors, ...)
  id <- unique(d[[1L]])

  if (length(id) != length(d[[1L]]) || !is.character(id)) {
    stop("PhenoData file has to have unique sample ids in the first column.")
  }

  row.names(d) <- id
  AnnotatedDataFrame(d[-1L], dimLabels=c("sampleNames", "sampleColumns"))
}
