## PREPROTRANSFORMATIONS

setClass("BaseClass", representation(objectname="character", objectoperation="character", isValid="logical", phase="PhaseClass", data="DataClass"),
         prototype(isValid=FALSE))

setGeneric("transformdata", function(object, dataobject) {
  standardGeneric("transformdata")
})

addPreprocessor <- function(classname, operation, phase){

  setClass(classname, contains="BaseClass", prototype=prototype(objectname=classname, objectoperation=operation, phase=phase))

  setMethod("transformdata", signature(object = classname), function(object, dataobject) {

    functionexpression <- gsub("data", "dataobject@x", operation)

    temp_x <- eval(parse(text=functionexpression))
    temp_y1 <- dataobject@y

    if (nrow(temp_x)!=nrow(dataobject@x)) {temp_y1 <- temp_y1[as.integer(rownames(temp_x))]}
    a <- data.frame(x=temp_x, y=temp_y1)
    a <- initializedataobject(a)
    a
  })

}

addPreprocessor("naomit", "na.omit(data)", phase=imputation)
library(DMwR)
addPreprocessor("knnimpute", "knnImputation(data, k=5)", phase=imputation)
addPreprocessor("scale", "scale(data)", phase=scaling)
addPreprocessor("centerscale", "scale(data, center=FALSE)", phase=scaling)
addPreprocessor("noscale", "identity(data)", phase=scaling)

## This initialization can be done

naomit1 <- new("naomit")
addtophase(imputation) <- naomit1
knnimpute1 <- new("knnimpute")
addtophase(imputation) <- knnimpute1
basescaling1 <- new("scale")
addtophase(scaling) <- basescaling1
centering1 <- new("centerscale")
addtophase(scaling) <- centering1
noscaling <- new("noscale")
addtophase(scaling) <- noscaling
