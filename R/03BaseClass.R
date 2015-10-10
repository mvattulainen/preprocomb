#' @include 02PhaseClass.R
NULL

# SUBCLASSES =================================================

#' PreprocessorClass
#'
#' PreprocessorClass is an abstract class from which concrete preprocessor (sub) classes are inhereted.
#' Inheritance is controlled by setpreprocessor() function.
#'
#' @slot objectname (character) object name
#' @slot objectoperation (character) operation (expression as character string)
#' @slot data (DataClass) object
#' @slot classificationaccuracy (numeric) classification accuracy
#' @slot hopkinsstatistic (numeric) clustering tendency
#' @slot LOFskewness (numeric) skewness value of LOF scores
#' @slot callhistory (character) vector of current and previous calls
#' @export

setClass("PreprocessorClass", representation(objectname="character", objectoperation="character", data="DataClass", classificationaccuracy="numeric", hopkinsstatistic="numeric", LOFskewness="numeric", callhistory="character"))

#' transformdata
#'
#' transformdata is a generic function. Its methods are defined by setpreprocessor().
#' The function is intented for package internal use.
#' @param object (PreprocessorClass) object
#' @param dataobject (DataClass/data frame) object
#' @export

setGeneric("transformdata", function(object, dataobject) {
  standardGeneric("transformdata")
})

#' setpreprocessor
#'
#' setpreprocessor is a constructor function for defining a preprocessor. The main
#' argument is the operation that is executed to transform the data such as "na.omit(basedata)"
#' for removing rows that have missing values. An operation can process either only the numeric
#' columns or also the class label column.
#'
#' If an operation deletes rows from numeric columns, the corresponding class
#' labels are deleted automatically. If an operation uses both numeric columns
#' and class labels, the defined operation must return both.
#'
#' @param classname (character)
#' @param operation (expression as character string)
#' @param mode (character) default to "numeric" for operation to be computed on data frame with numeric variables, option="all" for DataClass object with S4 slots x (numeric variables) and y (factor of class labels)
#' @return NULL definition of S4 class derived from PreprocessorClass and corresponding transformdata-method
#' @examples
#' ## Set of examples using only numeric variables and no class labels
#' ## setpreprocessor("naomit", "na.omit(basedata)")
#' ## setpreprocessor("scale", "scale(basedata,center=FALSE)")
#' ## setpreprocessor("nooutlierremove", "identity(basedata)")
#' ## setpreprocessor("softmaxscale", "data.frame(apply(basedata, 2, DMwR::SoftMax))")
#' ##
#' ## An example using also class labels and a supporting function
#' ## setpreprocessor("randomforestimpute", "rfimputefunc(basedata)", mode="all")
#' @export

setpreprocessor <- function(classname, operation, mode="numeric"){

  setClass(classname, contains="PreprocessorClass", where=topenv(parent.frame()), prototype=prototype(objectname=classname, objectoperation=operation))

  setMethod("transformdata", where=topenv(parent.frame()), signature(object = classname), function(object, dataobject) {

    if (mode=="numeric") {replacer <- "dataobject@x"}
    if (mode=="all") {replacer <- "dataobject"}

    functionexpression <- gsub("basedata", replacer, operation)

    output <- eval(parse(text=functionexpression))

    output_x <- output # DataClass object
    if (mode=="all") {output_x <- output@x} # data frame

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

  if (is(dataobject, "PreprocessorClass")==TRUE) {transformeddata <- transformdata(subclassobject, dataobject@data)}

  #else {stop("Argument 'dataobject' must be either a DataClass object or object inhereted from PreprocessorClass")}

  subclassobject@data <- transformeddata

  if (validate==TRUE) {
    subclassobject@data <- validatedataclassobject(transformeddata)
  }

  return(subclassobject)

}

#' prc
#'
#' prc is the main function for interactive use. It takes data, transforms it according to the given
#' preprocessor and computes statistics of the transformed data. The main use case is the chaining of
#' the preprocessors as show in the examples below.
#'
#' @param classname (character) name of preprocessor (i.e. PreprocessorClass sub class as defined by setpreprocessor())
#' @param dataobject (sub class/ data frame/ DataClass) object
#' @param model (character) caret model name, note: the required model library must be attached, defaults to "knn"
#' @param nholdout (integer) number of holdout rounds used in computation of classification accuracy, must be two or more, defaults to two
#' @param nsharehopkins (integer) denominator for sample size for hopkins statistics, defauls to three  (n=nrow(data)/3)
#' @param klof (integer) number of data points used for neighborhood in LOF algorithm, defaults to five
#' @return object of PreprocessorClass sub class
#' @examples
#' ## a <- prc("scale", iris)
#' ## b <- prc("rfselect75", a)
#' ## d <- prc("scale", iris, "rf", 20, 2, 10)
#' @export

prc <- function(classname, dataobject, model="knn", nholdout=2, nsharehopkins=3, klof=5){

  predictor <- model

  subclassobject <- new(classname)

  if (class(dataobject)=="DataClass") {
    transformeddata <- transformdata(subclassobject, dataobject)
    subclassobject@callhistory <- subclassobject@objectname
    }

  if (class(dataobject)=="data.frame") {
    transformeddata <- transformdata(subclassobject, initializedataclassobject(dataobject))
    subclassobject@callhistory <- subclassobject@objectname
    }

  if (is(dataobject, "PreprocessorClass")==TRUE) {
    transformeddata <- transformdata(subclassobject, dataobject@data)
    subclassobject@callhistory <- c(dataobject@callhistory, subclassobject@objectname)
  }

  subclassobject@data <- transformeddata

  subclassobject@data <- validatedataclassobject(transformeddata)

  subclassobject@classificationaccuracy <- suppressWarnings(subclassprediction(subclassobject, predictor, nholdout))

  subclassobject@hopkinsstatistic <- unname(unlist(clustertend::hopkins((subclassobject@data)@x, n=as.integer(nrow((subclassobject@data)@x)/nsharehopkins)   )))

  subclassobject@LOFskewness <- e1071::skewness(DMwR::lofactor((subclassobject@data)@x, k=klof))

  return(subclassobject)

}

setMethod("show", signature(object = "PreprocessorClass"), function(object){
  cat("# OBJECT:", "\n")
  cat("# class:", class(object), "\n")
  cat("# call history:", object@callhistory, "\n")
  cat("\n")
  cat("# COMPUTATIONS:", "\n")
  cat("# classification accuracy:", round(object@classificationaccuracy, 2), "\n")
  cat("# hopkins statistic, clustering tendency:", round(object@hopkinsstatistic, 2), "\n")
  cat("# skewness of LOF scores, outlier tendency:", round(object@LOFskewness, 2), "\n")
  cat("\n")
  cat("# FITNESS FOR MODEL FITTING:", "\n")
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

#' getpreprocessors
#'
#' gets the available preprocessors, that is: PreprocessorClass (sub) classes.
#' Shown preprocessors can be used by functions prc() and setphase().
#' @export

getpreprocessors <- function() {names(getClass("PreprocessorClass")@subclasses)}

#' testpreprocessors
#'
#' run a test for preprocessors
#' @param preprocessors (character) vector of preprocessors, by default gets all preprocessors with getpreprocessors()
#' @param data (data frame) to be tested against, defaults to random data frame without missing values
#' @export

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


