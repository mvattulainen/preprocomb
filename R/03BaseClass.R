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

    temp_x <- eval(parse(text=functionexpression))
    temp_y1 <- dataobject@y

    if (nrow(temp_x)!=nrow(dataobject@x)) {temp_y1 <- temp_y1[as.integer(rownames(temp_x))]} # If rows has been deleted
    a <- data.frame(x=temp_x, y=temp_y1)
    a <- initializedataobject(a)
    a
  })

}

# Imputation
library(DMwR)
library(randomForest)
addPreprocessor("naomit", "na.omit(basedata)")
addPreprocessor("meanimpute", "data.frame(apply(basedata, 2, meanrep))")
addPreprocessor("knnimpute", "knnImputation(basedata, k=5)")
addPreprocessor("rfimpute", "suppressMessages(rfImpute(basedata@y ~ ., basedata@x))", mode="all")

## Scaling
addPreprocessor("scale", "scale(basedata)")
addPreprocessor("centerscale", "scale(basedata, center=FALSE)")
addPreprocessor("noscale", "identity(basedata)")
addPreprocessor("minmaxscale", "data.frame(apply(basedata, 2, range01))")
addPreprocessor("softmax", "data.frame(apply(basedata, 2, SoftMax))")

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

imputation <- initializephaseclassobject("imputation", list(naomit1, meanimpute1, knnimpute1, rfimpute1), TRUE)
scaling <- initializephaseclassobject("scaling", list(basescaling1, centering1, noscaling, minmaxscaling,softmaxscaling), FALSE)
