#' @include 02PhaseClass.R
NULL

# SUBCLASSES =================================================

setClass("BaseClass", representation(objectname="character", objectoperation="character", data="DataClass", classificationerror="numeric", hopkinsstatistic="numeric", LOFskewness="numeric", callhistory="character"))

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

prepro <- function(classname, dataobject, validate=FALSE){

  subclassobject <- new(classname)

  if (class(dataobject)=="DataClass") {transformeddata <- transformdata(subclassobject, dataobject)}

  if (class(dataobject)=="data.frame") {transformeddata <- transformdata(subclassobject, initializedataclassobject(dataobject))}

  if (is(dataobject, "BaseClass")==TRUE) {transformeddata <- transformdata(subclassobject, dataobject@data)}

  #else {stop("Argument 'dataobject' must be either a DataClass object or object inhereted from BaseClass")}

  subclassobject@data <- transformeddata

  if (validate==TRUE) {
    subclassobject@data <- validatedataclassobject(transformeddata)
  }

  return(subclassobject)

}


prc <- function(classname, dataobject, predictor="knn", nholdout=2, nsharehopkins=3, klof=5){

  subclassobject <- new(classname)

  if (class(dataobject)=="DataClass") {
    transformeddata <- transformdata(subclassobject, dataobject)
    subclassobject@callhistory <- subclassobject@objectname
    }

  if (class(dataobject)=="data.frame") {
    transformeddata <- transformdata(subclassobject, initializedataclassobject(dataobject))
    subclassobject@callhistory <- subclassobject@objectname
    }

  if (is(dataobject, "BaseClass")==TRUE) {
    transformeddata <- transformdata(subclassobject, dataobject@data)
    subclassobject@callhistory <- c(dataobject@callhistory, subclassobject@objectname)
  }

  subclassobject@data <- transformeddata

  subclassobject@data <- validatedataclassobject(transformeddata)

  subclassobject@classificationerror <- suppressWarnings(subclassprediction(subclassobject, predictor, nholdout))

  subclassobject@hopkinsstatistic <- unname(unlist(clustertend::hopkins((subclassobject@data)@x, n=as.integer(nrow((subclassobject@data)@x)/nsharehopkins)   )))

  subclassobject@LOFskewness <- skewness(DMwR::lofactor((subclassobject@data)@x, k=klof))

  return(subclassobject)

}


setMethod("show", signature(object = "BaseClass"), function(object){
  cat("# OBJECT:", "\n")
  cat("# class:", class(object), "\n")
  cat("# call history:", object@callhistory, "\n")
  cat("\n")
  cat("# COMPUTATIONS:", "\n")
  cat("# misclassification error:", round(object@classificationerror, 2), "\n")
  cat("# hopkins statistic, clustering tendency:", round(object@hopkinsstatistic, 2), "\n")
  cat("# skewness of LOF scores, outlier tendency:", round(object@LOFskewness, 2), "\n")
  cat("\n")
  cat("# DATA VALIDITY:", "\n")
  cat("# variance in all variables:", object@data@variance, "\n")
  cat("# only finite values:", object@data@finite, "\n")
  cat("# complete observations:", object@data@completeobs, "\n")
  cat("# class balance:", object@data@classbalance, "\n")
  cat("# not multicollinear above .9:", object@data@corrbelowdotnine, "\n")
  cat("# n to p ratio more than 2:", object@data@ntopratiotwoplus, "\n")
  cat("# 3 or more predictors and more than 20 observations:", object@data@mindimensions, "\n")
  } )

# DEFAULT PREPROCESSORS AND PHASES ==========================

# NO ACTION

setpreprocessor("noaction", "identity(basedata)")

# Low variance

setpreprocessor("nearzerovar", "nzv(basedata)")

# Imputation

setpreprocessor("naomit", "na.omit(basedata)")
setpreprocessor("meanimpute", "data.frame(apply(basedata, 2, meanrep))")
setpreprocessor("knnimpute", "knnimputefunc(basedata)")
setpreprocessor("randomforestimpute", "rfimputefunc(basedata)", mode="all")

## Scaling
setpreprocessor("scale", "scale(basedata,center=FALSE)")
setpreprocessor("centerscale", "scale(basedata, center=TRUE)")
setpreprocessor("minmaxscale", "data.frame(apply(basedata, 2, range01))")
setpreprocessor("softmaxscale", "data.frame(apply(basedata, 2, DMwR::SoftMax))")

# Outlier removal
# setpreprocessor("lof", "lofcut(basedata)")
setpreprocessor("orhoutlier", "orhcut(basedata)")

# Class imbalance

setpreprocessor("oversample", "oversample(basedata)", mode="all")

# Smoothing

setpreprocessor("lowesssmooth", "smoothlowess(basedata)")


# Feature selection

setpreprocessor("rfselect75", "rfimportance(basedata, .25)", mode="all")
setpreprocessor("rfselect50", "rfimportance(basedata, .50)", mode="all")

# Class imbalance

setpreprocessor("smotesample", "smotesample(basedata)", mode="all")
setpreprocessor("oversample", "oversample(basedata)", mode="all")
setpreprocessor("undersample", "undersample(basedata)", mode="all")

# Phases

imputation <- setphase("imputation", c("naomit", "meanimpute", "knnimpute", "randomforestimpute"), TRUE)
variance <- setphase("variance", c("noaction", "nearzerovar"), FALSE)
smoothing <- setphase("smoothing", c("noaction", "lowesssmooth"), FALSE)
scaling <- setphase("scaling", c("noaction", "scale", "centerscale", "minmaxscale", "softmaxscale"), FALSE)
outlier <- setphase("outlier", c("noaction", "orhoutlier"), FALSE)
sampling <- setphase("imbalance", c("noaction", "oversample", "undersample", "smotesample"), FALSE)
selection <- setphase("selection", c("noaction", "rfselect50", "rfselect75"), FALSE)


### BASETEST ==========

getphases <- function() {
  temp <- as.list(ls())
  out <- lapply(temp, function(x) class(eval(as.name(x)))=="PhaseClass")
  }

getpreprocessors <- function() {names(getClass("BaseClass")@subclasses)}

testpreprocessors <- function(preprocessors=NULL, data=NULL){
  if (is.null(preprocessors)) {preprocessors <- getpreprocessors() }
  if (is.null(data)) {data <- data.frame(matrix(rbinom(4*30, 1, .5), ncol=4), class=sample(letters[1:2], 30, replace=TRUE))}
  cls <- as.list(preprocessors)
  testdata <- initializedataclassobject(data)
  temp <- lapply(cls, function(x) prepro(x, testdata))
  temp1 <- lapply(temp, function(x) slot(x, "data"))
  print(reportexitstatus(temp1))
  return(temp1)
  }


