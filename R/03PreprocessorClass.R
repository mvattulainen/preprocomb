#' @include 02PhaseClass.R
NULL

# SUBCLASSES =================================================

#' an abstract class from which concrete preprocessor (sub) classes are inhereted.
#'
#' Inheritance is controlled by setpreprocessor() function.
#'
#' @slot objectname (character) object name
#' @slot objectoperation (character) operation (expression as character string)
#' @slot data (DataClass) object
#' @slot classificationaccuracy (numeric) classification accuracy
#' @slot hopkinsstatistic (numeric) clustering tendency
#' @slot ORHskewness (numeric) skewness value of ORH scores
#' @slot callhistory (character) vector of current and previous calls
#' @export

setClass("PreprocessorClass", representation(objectname="character", objectoperation="character", data="DataClass", classificationaccuracy="numeric", hopkinsstatistic="numeric", ORHskewness="numeric", callhistory="character"))

#' transformdata
#'
#' transformdata is a generic function. Its methods are defined by setpreprocessor().
#' The function is intented for package internal use, but exported so that classes can be inhereted from it.
#' @param object (PreprocessorClass) object
#' @param dataobject (DataClass/data frame) object
#' @export

setGeneric("transformdata", function(object, dataobject) {
  standardGeneric("transformdata")
})

#' constructor function for adding a new preprocessing technique to the system
#'
#' The main argument is the operation that is executed to transform the data such as "na.omit(basedata)"
#' for removing rows that have missing values. An operation can process either only the numeric
#' columns or also the class label column.
#'
#' Preprocessing techniques defined with setpreprocessor() can be combined to a phase.
#' Phases defined with setphase() can be combined to a grid of combinations with setgrid().
#'
#' @param classname (character)
#' @param operation (expression as character string)
#' @return NULL, side-effect: definition of S4 class derived from PreprocessorClass and corresponding transformdata-method
#' @details The user-defined S4 class definitions are stored in global environment and thus the function can not be used from an other package.
#'
#' scaleexample <- function(dataobject) dataobject <- initializedataclassobject(data.frame(x=scale(dataobject@@x), dataobject@@y))
#' setpreprocessor("scaleexample", "scaleexample(dataobject)")
#'
#' @export

setpreprocessor <- function(classname, operation){

  # save operation to specific environment

  storagepositition <- paste("preprocessordefinitionstorage$", classname, sep="")
  funcbody <- as.character(eval(parse(text=paste("body(", gsub( "\\(.*$", "", operation ), ")", sep=""))))
  storagesaving <- paste(storagepositition, "<-'", paste(funcbody, collapse=""), "'", sep="")
  eval <- eval(parse(text=storagesaving))

  # Create a subclass of PreprocessorClass

  setClass(classname, contains="PreprocessorClass", where=topenv(parent.frame()), prototype=prototype(objectname=classname, objectoperation=operation))

  # Define tranformdata method for the subclass

  setMethod("transformdata", where=topenv(parent.frame()), signature(object = classname), function(object, dataobject) {

    output <- eval(parse(text=operation))

    return(output)

  })

}


#' the MAIN function for interactive use.
#'
#' prepro() takes data, transforms it according to the given preprocessor and computes statistics of the
#' transformed data. The main use case is the chaining of the preprocessors as show in the examples below.

#' @param dataobject (sub class/ data frame/ DataClass) object
#' @param classname (character) name of preprocessor (i.e. PreprocessorClass sub class as defined by setpreprocessor())
#' @param model (character) caret model name, note: the required model library must be attached, defaults to "rpart"
#' @param nholdout (integer) number of holdout rounds used in computation of classification accuracy, must be two or more, defaults to 2
#' @param cores (integer) number of cores used in parallel processing of holdout rounds, defaults to 1
#' @return object of PreprocessorClass sub class
#' @examples
#' ## a <- prepro(iris, "basicscale")
#' ## b <- prepro(a, "rfselect75")
#' ## d <- prepro(iris, "basicscale", "rf", nholdout=20, cores=2)
#' @details If a data object has missing values, one of the imputation preprocessors must be applied first.

#' @export


prepro <- function(dataobject, classname, model="rpart", nholdout=2, cores=1){

  doParallel::registerDoParallel(cores)

  predictor <- model

  subclassobject <- new(classname)

  # Preprocess data

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
  subclassobject@data <- validatedata(transformeddata)

  # Compute classification accuracy

  data <- subclassobject@data
  temp <- data.frame(x=data@x, y=data@y)
  temp <- getprogrammaticprediction(temp, predictor, nholdout)
  temp <- apply(temp, 2, mean)[1]
  subclassobject@classificationaccuracy <- temp

  # Compute clustering tendency

  temp <- clustertend::hopkins(data@x, n=nrow(data@x)-1)
  subclassobject@hopkinsstatistic <- unname(unlist(temp))

  # Compute outlier scores

  orh_score <- suppressMessages(DMwR::outliers.ranking((subclassobject@data)@x))
  orh_rank <- orh_score$prob.outliers[orh_score$rank.outliers]
  subclassobject@ORHskewness <- e1071::skewness(orh_rank)

  doParallel::stopImplicitCluster()

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
  cat("# skewness of ORH scores, outlier tendency:", round(object@ORHskewness, 2), "\n")
  cat("\n")
  cat("# FITNESS FOR MODEL FITTING:", "\n")
  cat("# variance in all variables:", object@data@variance, "\n")
  cat("# only finite values:", object@data@finite, "\n")
  cat("# complete observations:", object@data@completeobs, "\n")
  cat("# class balance:", object@data@classbalance, "\n")
  cat("# n to p ratio more than 2:", object@data@ntopratiotwoplus, "\n")
  cat("# 3 or more predictors and more than 20 observations:", object@data@mindimensions, "\n")
  } )



### BASETEST ==========

#' gets preprocessor definition
#'
#' @param preprocessor (character) name of preprocessor, defaults to NULL for list of all preprocessors
#'
#' @details
#' getpreprocessor with the name of the preprocessor shows preprocessing function body defined with setpreprocessor().\cr
#' getpreprocessor without name shows all preprocessors that can be used by functions prepro() and setphase().
#' @examples
#' getpreprocessor()
#' getpreprocessor("basicscale")
#' @export

getpreprocessor <- function(preprocessor=NULL)   {

  if (is.null(preprocessor)) {names(getClass("PreprocessorClass")@subclasses)}
  else {
    getdef <- paste("preprocessordefinitionstorage$",preprocessor)
    defres <- eval(parse(text=getdef))
    writeLines(defres)
  }

}

#' test preprocessing techniques against data
#'
#' Intended to be used when adding new preprocessing techniques with setpreprocessor().
#'
#' @param preprocessors (character) vector of preprocessors, by default gets all preprocessors with getpreprocessors()
#' @param data (data frame) to be tested against, defaults to random data frame without missing values
#' @examples
#' testpreprocessors()
#' @export

testpreprocessors <- function(preprocessors=NULL, data=NULL){
  if (is.null(preprocessors)) {preprocessors <- getpreprocessor() }
  if (is.null(data)) {data <- data.frame(matrix(rbinom(4*30, 1, .5), ncol=4), class=sample(letters[1:2], 30, replace=TRUE))}
  cls <- as.list(preprocessors)
  testdata <- initializedataclassobject(data)
  temp <- lapply(cls, function(x) initializedataslot(x, testdata))
  temp1 <- lapply(temp, function(x) slot(x, "data"))
  print(reportexitstatus(temp1))
  return(temp1)
  }


