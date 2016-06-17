setGeneric("dilutionData", function(object) standardGeneric("dilutionData"))
setGeneric("dilutionNames", function(object) standardGeneric("dilutionNames"))
setGeneric("replicateData", function(object) standardGeneric("replicateData"))
setGeneric("replicateNames", function(object) standardGeneric("replicateNames"))

setGeneric("FlexmapSet",
  function(assayData,
           phenoData=annotatedDataFrameFrom(assayData, dimname="sample"),
           featureData=annotatedDataFrameFrom(assayData, dimname="feature"),
           dilutionData=annotatedDataFrameFrom(assayData, dimname="dilution"),
           replicateData=annotatedDataFrameFrom(assayData, dimname="replicate"),
           experimentData=MIAME(),
           annotation=character(),
           protocolData=annotatedDataFrameFrom(assayData, dimname="sample"), ...)
  standardGeneric("FlexmapSet"),
  signature="assayData")
