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

    output <- eval(parse(text=functionexpression))

    output_x <- output
    if (mode=="all") {output_x <- output@x}

    rownames(output_x) <- seq(1,nrow(output_x),1)

    output_y <- dataobject@y # if rows have not been removed
    if (mode=="all") {output_y <- output@y}

    if (nrow(output_x) < nrow(dataobject@x)) { # if rows have been deleted
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
addPreprocessor("knnimpute", "DMwR::knnImputation(basedata, k=5)")
addPreprocessor("randomforestimpute", "rfimputefunc(basedata)", mode="all")

## Scaling
addPreprocessor("scale", "scale(basedata,center=FALSE)")
addPreprocessor("centerscale", "scale(basedata, center=TRUE)")
addPreprocessor("noscale", "identity(basedata)")
addPreprocessor("minmaxscale", "data.frame(apply(basedata, 2, range01))")
addPreprocessor("softmax", "data.frame(apply(basedata, 2, DMwR::SoftMax))")

# Outlier removal
addPreprocessor("lof", "lofcut(basedata)")
addPreprocessor("orh", "orhcut(basedata)")
addPreprocessor("nooutremove", "identity(basedata)")

# Class imbalance

addPreprocessor("oversample", "rfimputefunc(basedata)", mode="all")
addPreprocessor("nosample", "identity(basedata)")


# Feature selection

addPreprocessor("rfvarused", "rfimportance(basedata)", mode="all")
addPreprocessor("novarused", "identity(basedata)")

## This initialization can be done


naomit1 <- new("naomit")
knnimpute1 <- new("knnimpute")
meanimpute1 <- new("meanimpute")
rfimpute1 <- new("randomforestimpute")

basescaling1 <- new("scale")
centering1 <- new("centerscale")
noscaling <- new("noscale")
minmaxscaling <- new("minmaxscale")
softmaxscaling <- new("softmax")

lof <- new("lof")
orh <- new("orh")
noout <- new("nooutremove")

oversample1 <- new("oversample")
nosample1 <- new("nosample")

varused1 <- new("rfvarused")
novarused1 <- new("novarused")

## TESTS

imputation <- initializephaseclassobject("imputation", list(naomit1, meanimpute1, knnimpute1, rfimpute1), TRUE)
scaling <- initializephaseclassobject("scaling", list(basescaling1, centering1, noscaling, minmaxscaling,softmaxscaling), FALSE)
outlier <- initializephaseclassobject("outlier", list(lof, orh, noout), FALSE)
sampling <- initializephaseclassobject("sampling", list(oversample1, nosample1), FALSE)
selection <- initializephaseclassobject("selection", list(varused1, novarused1), FALSE)
