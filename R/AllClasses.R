#' @import Biobase
.FlexmapSet <- setClass("FlexmapSet",
                        slots=list(exprs="array"),
                        contains="eSet",
                        prototype=prototype(
                          new("VersionedBiobase",
                                versions=c(Biobase::classVersion("eSet"),
                                           FlexmapSet="1.0.0")))
)
