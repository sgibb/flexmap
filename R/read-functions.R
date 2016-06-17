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
