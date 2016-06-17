.FlexmapSet <- setClass("FlexmapSet",
                        slots=list(dilutionData="AnnotatedDataFrame",
                                   replicateData="AnnotatedDataFrame"),
                        contains="eSet",
                        prototype=prototype(
                          new("VersionedBiobase",
                                versions=c(Biobase::classVersion("eSet"),
                                           FlexmapSet="1.0.0")))
)
