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

addPreprocessor <- function(classname, operation){

  setClass(classname, contains="BaseClass", prototype=prototype(objectname=classname, objectoperation=operation))

  setMethod("transformdata", signature(object = classname), function(object, dataobject) {

    functionexpression <- gsub("x", "dataobject@x", operation)

    temp_x <- eval(parse(text=functionexpression))
    temp_y1 <- dataobject@y

    if (nrow(temp_x)!=nrow(dataobject@x)) {temp_y1 <- temp_y1[as.integer(rownames(temp_x))]} # If rows has been deleted
    a <- data.frame(x=temp_x, y=temp_y1)
    a <- initializedataobject(a)
    a
  })

}

# Imputation
addPreprocessor("naomit", "na.omit(x)")
library(DMwR)
addPreprocessor("knnimpute", "knnImputation(x, k=5)")

## Scaling
addPreprocessor("scale", "scale(x)")
addPreprocessor("centerscale", "scale(x, center=FALSE)")
addPreprocessor("noscale", "identity(x)")
addPreprocessor("minmaxscale", "data.frame(apply(x, 2, range01))")

## This initialization can be done

naomit1 <- new("naomit")
knnimpute1 <- new("knnimpute")

basescaling1 <- new("scale")
centering1 <- new("centerscale")
noscaling <- new("noscale")
minmaxscaling <- new("minmaxscale")

imputation <- initializephaseclassobject("imputation", list(naomit1, knnimpute1), TRUE)
scaling <- initializephaseclassobject("scaling", list(basescaling1, centering1, noscaling, minmaxscaling), FALSE)
