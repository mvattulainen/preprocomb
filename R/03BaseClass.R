## PREPROTRANSFORMATIONS

#' BaseClass
#'
#' BaseClass represents the preprocessors
#' @slot objectname (character) name of the object
#' @slot objectoperation (expression as character string) expression to be computed
#' @slot isvalid (logical) TRUE, if computation of objectoperation is successful
#' @slot data (DataClass) data to be used in object operation computation

setClass("BaseClass", representation(objectname="character", objectoperation="character", isvalid="logical", data="DataClass"),
         prototype(isvalid=FALSE))

setGeneric("transformdata", function(object, dataobject) {
  standardGeneric("transformdata")
})

#' addpreprocessor
#'
#' addpreprocessor is a helper function for inheriting subclasses from BaseClass
#' @param classname (character)
#' @param operation (expression as character string)
#' @param mode (character) default to "numeric" for operation to be computed on data frame with numeric variables, option="all" for DataClass object with slots x (numeeric variables) and y (factor of class labels)

addpreprocessor <- function(classname, operation, mode="numeric"){

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

addpreprocessor("naomit", "na.omit(basedata)")
addpreprocessor("meanimpute", "data.frame(apply(basedata, 2, meanrep))")
addpreprocessor("knnimpute", "DMwR::knnImputation(basedata, k=5)")
addpreprocessor("randomforestimpute", "rfimputefunc(basedata)", mode="all")

## Scaling
addpreprocessor("scale", "scale(basedata,center=FALSE)")
addpreprocessor("centerscale", "scale(basedata, center=TRUE)")
addpreprocessor("noscale", "identity(basedata)")
addpreprocessor("minmaxscale", "data.frame(apply(basedata, 2, range01))")
addpreprocessor("softmaxscale", "data.frame(apply(basedata, 2, DMwR::SoftMax))")

# Outlier removal
addpreprocessor("lof", "lofcut(basedata)")
addpreprocessor("orh", "orhcut(basedata)")
addpreprocessor("nooutlierremove", "identity(basedata)")

# Class imbalance

addpreprocessor("oversample", "rfimputefunc(basedata)", mode="all")
addpreprocessor("nosample", "identity(basedata)")


# Feature selection

addpreprocessor("rfvarused", "rfimportance(basedata)", mode="all")
addpreprocessor("noselection", "identity(basedata)")

imputation <- initializephaseclassobject("imputation", list("naomit", "meanimpute", "knnimpute", "randomforestimpute"), TRUE)
scaling <- initializephaseclassobject("scaling", list("noscale", "scale", "centerscale", "minmaxscale", "softmaxscale"), FALSE)
outlier <- initializephaseclassobject("outlier", list("nooutlierremove", "lof", "orh"), FALSE)
sampling <- initializephaseclassobject("sampling", list("nosample", "oversample"), FALSE)
selection <- initializephaseclassobject("selection", list("noselection", "rfvarused"), FALSE)
