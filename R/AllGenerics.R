setGeneric("dilutionData", function(object) standardGeneric("dilutionData"))
setGeneric("dilutionData<-", function(object, value)
           standardGeneric("dilutionData<-"))
setGeneric("dilutionNames", function(object) standardGeneric("dilutionNames"))
setGeneric("replicateData", function(object) standardGeneric("replicateData"))
setGeneric("replicateData<-", function(object, value)
           standardGeneric("replicateData<-"))
setGeneric("replicateNames", function(object) standardGeneric("replicateNames"))
setGeneric("processingData", function(object) standardGeneric("processingData"))
setGeneric("processingData<-", function(object, value, ...) standardGeneric("processingData<-"))

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
