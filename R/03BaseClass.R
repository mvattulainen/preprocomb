#' @include 02PhaseClass.R
NULL

# SUBCLASSES =================================================

setClass("BaseClass", representation(objectname="character", objectoperation="character", isvalid="logical", data="DataClass"),
         prototype(isvalid=FALSE))

setGeneric("transformdata", function(object, dataobject) {
  standardGeneric("transformdata")
})

#' setpreprocessor
#'
#' setpreprocessor is a helper function used to define sub classes of BaseClass.
#' Specifically, sub classes include the operation to be executed to preprocess data.
#'
#' If operation deletes rows in numeric data such as outliers, the corresponding class
#' labels are deleted automatically.
#'
#' If operation uses both numeric columns and class labels, the defined operation must
#' return both.
#'
#'
#' @param classname (character)
#' @param operation (expression as character string)
#' @param mode (character) default to "numeric" for operation to be computed on data frame with numeric variables, option="all" for DataClass object with slots x (numeeric variables) and y (factor of class labels)
#' @examples
#' ## Set of examples using only numeric variables and no class labels
#' ## setpreprocessor("naomit", "na.omit(basedata)")
#' ## setpreprocessor("scale", "scale(basedata,center=FALSE)")
#' ## setpreprocessor("nooutlierremove", "identity(basedata)")
#' ## setpreprocessor("softmaxscale", "data.frame(apply(basedata, 2, DMwR::SoftMax))")
#' ##
#' ## An example using also class labels and a supporting function
#' ## setpreprocessor("randomforestimpute", "rfimputefunc(basedata)", mode="all")


setpreprocessor <- function(classname, operation, mode="numeric"){

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
    newdataobject <- initializedataclassobject(transformeddata)
    newdataobject
  })

}

initializesubclassobject <- function(classname, dataobject, validate=FALSE){

  subclassobject <- new(classname)

  if (class(dataobject)=="DataClass") {transformeddata <- transformdata(subclassobject, dataobject)}

  if (class(dataobject)=="data.frame") {transformeddata <- transformdata(subclassobject, initializedataclassobject(dataobject))}

  if (is(dataobject, "BaseClass")==TRUE) {transformeddata <- transformdata(subclassobject, dataobject@data)}

  #else {stop("Argument 'dataobject' must be either a DataClass object or object inhereted from BaseClass")}

  subclassobject@data <- transformeddata

  if (validate==TRUE) {
    validateddata <- validatedataclassobject(transformeddata)
    subclassobject@data <- validateddata
  }



  return(subclassobject)

}

# DEFAULT PREPROCESSORS AND PHASES ==========================

# Imputation

setpreprocessor("naomit", "na.omit(basedata)")
setpreprocessor("meanimpute", "data.frame(apply(basedata, 2, meanrep))")
setpreprocessor("knnimpute", "knnimputefunc(basedata)")
setpreprocessor("randomforestimpute", "rfimputefunc(basedata)", mode="all")

## Scaling
setpreprocessor("scale", "scale(basedata,center=FALSE)")
setpreprocessor("centerscale", "scale(basedata, center=TRUE)")
setpreprocessor("noscale", "identity(basedata)")
setpreprocessor("minmaxscale", "data.frame(apply(basedata, 2, range01))")
setpreprocessor("softmaxscale", "data.frame(apply(basedata, 2, DMwR::SoftMax))")

# Outlier removal
# setpreprocessor("lof", "lofcut(basedata)")
setpreprocessor("orh", "orhcut(basedata)")
setpreprocessor("nooutlierremove", "identity(basedata)")

# Class imbalance

setpreprocessor("oversample", "oversample(basedata)", mode="all")
setpreprocessor("nosample", "identity(basedata)")


# Feature selection

setpreprocessor("rfimp75", "rfimportance(basedata, .25)", mode="all")
setpreprocessor("rfimp50", "rfimportance(basedata, .50)", mode="all")
setpreprocessor("noselection", "identity(basedata)")

# Phases

imputation <- setphase("imputation", c("naomit", "meanimpute", "knnimpute", "randomforestimpute"), TRUE)
scaling <- setphase("scaling", c("noscale", "scale", "centerscale", "minmaxscale", "softmaxscale"), FALSE)
outlier <- setphase("outlier", c("nooutlierremove", "orh"), FALSE)
sampling <- setphase("sampling", c("nosample", "oversample"), FALSE)
selection <- setphase("selection", c("noselection", "rfimp50", "rfimp75"), FALSE)

### BASETEST ==========

getpreprocessors <- function() {names(getClass("BaseClass")@subclasses)}

testpreprocessors <- function(preprocessors=NULL, data=NULL){
  if (is.null(preprocessors)) {preprocessors <- getpreprocessors() }
  if (is.null(data)) {data <- data.frame(matrix(rbinom(4*30, 1, .5), ncol=4), class=sample(letters[1:2], 30, replace=TRUE))}
  cls <- as.list(preprocessors)
  testdata <- initializedataclassobject(data)
  temp <- lapply(cls, function(x) initializesubclassobject(x, testdata))
  temp1 <- lapply(temp, function(x) slot(x, "data"))
  print(reportexitstatus(temp1))
  return(temp1)
  }


