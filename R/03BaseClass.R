## PREPROTRANSFORMATIONS

setClass("BaseClass", representation(objectname="character", objectoperation="character", isValid="logical", data="DataClass"),
         prototype(isValid=FALSE))

setMethod("getname", signature(object = "BaseClass"), function(object)
{
  object@objectname
})

setGeneric("transformdata", function(object, dataobject) {
  standardGeneric("transformdata")
})

addPreprocessor <- function(classname, operation, mode="numeric"){

  setClass(classname, contains="BaseClass", prototype=prototype(objectname=classname, objectoperation=operation))

  setMethod("transformdata", signature(object = classname), function(object, dataobject) {

    if (mode=="numeric") {functionexpression <- gsub("basedata", "dataobject@x", operation)}
    if (mode=="all") {functionexpression <- gsub("basedata", "dataobject", operation)}

    output_x <- eval(parse(text=functionexpression))
    rownames(output_x) <- seq(1,nrow(output_x),1)

    output_y <- dataobject@y # if rows have not been removed

    if (nrow(output_x)!=nrow(dataobject@x)) { # if rows have been deleted
      output_y <- output_y[as.integer(rownames(output_x))]}

    transformeddata <- data.frame(x=output_x, y=output_y)
    newdataobject <- initializedataobject(transformeddata)
    newdataobject
  })

}

# Imputation

library(randomForest)
addPreprocessor("naomit", "na.omit(basedata)")
addPreprocessor("meanimpute", "data.frame(apply(basedata, 2, meanrep))")
addPreprocessor("knnimpute", "knnImputation(basedata, k=5)")
addPreprocessor("rfimpute", "rfImpute(basedata@y ~ ., basedata@x)", mode="all")

## Scaling
addPreprocessor("scale", "scale(basedata)")
addPreprocessor("centerscale", "scale(basedata, center=FALSE)")
addPreprocessor("noscale", "identity(basedata)")
addPreprocessor("minmaxscale", "data.frame(apply(basedata, 2, range01))")
addPreprocessor("softmax", "data.frame(apply(basedata, 2, SoftMax))")

# Outlier removal
addPreprocessor("lof", "lofcut(basedata)")
addPreprocessor("nooutremove", "identity(basedata)")

## This initialization can be done


naomit1 <- new("naomit")
knnimpute1 <- new("knnimpute")
meanimpute1 <- new("meanimpute")
rfimpute1 <- new("rfimpute")

basescaling1 <- new("scale")
centering1 <- new("centerscale")
noscaling <- new("noscale")
minmaxscaling <- new("minmaxscale")
softmaxscaling <- new("softmax")

lof <- new("lof")
noout <- new("nooutremove")

imputation <- initializephaseclassobject("imputation", list(naomit1, meanimpute1, knnimpute1, rfimpute1), TRUE)
scaling <- initializephaseclassobject("scaling", list(basescaling1, centering1, noscaling, minmaxscaling,softmaxscaling), FALSE)
outlier <- initializephaseclassobject("outlier", list(lof, noout), FALSE)
