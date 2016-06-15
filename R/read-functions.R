#' Read flexmap csv file format
#'
#' In general it is: Sample; (Replicate); (Dilution); Antigen1/Feature1; ...;
#' FeatureN
#' @param file file path
#' @return array, dimensions = samples x features x dilution x replicates
#' @noRd
.readFlexmapCsv <- function(file) {
  header <- scan(file, what=character(), sep=";", nlines=1L, quiet=TRUE)
  content <- scan(file, what=c(character(1L),
                               replicate(length(header) - 1L, double())),
                  skip=as.numeric(is.character(file)),
                  sep=";", quiet=TRUE, multi.line=FALSE)

  dims <- c("sample", "replicate", "dilution")
  i <- setNames(match(dims, tolower(header), nomatch=0L), dims)

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
        perm=c(1L, 4L, 2L, 3L))
}
