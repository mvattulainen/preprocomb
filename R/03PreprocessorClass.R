#' @include 02PhaseClass.R
NULL

# SUBCLASSES =================================================

#' A virtual S4 class representing preprocessors
#'
#' Inheritance is controlled by setpreprocessor() function.
#'
#' @slot objectname (character) object name
#' @slot objectoperation (character) operation (R expression as character string)
#' @slot data (DataClass) object
#' @slot classificationaccuracy (numeric) classification accuracy
#' @slot hopkinsstatistic (numeric) clustering tendency
#' @slot callhistory (character) vector of current and previous calls
#' @export

setClass("PreprocessorClass", representation(objectname="character", objectoperation="character", data="DataClass", classificationaccuracy="numeric", hopkinsstatistic="numeric", callhistory="character"))

#' transformdata
#'
#' transformdata is a generic preprocessing function. Its methods are defined by setpreprocessor().
#' The function is intented for package internal use, but exported so that classes can be inhereted from it.
#' @param object (PreprocessorClass) object
#' @param dataobject (DataClass/data frame) object
#' @export
#' @keywords internal

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
#' @param classname (character) name of the preprocessing techniques
#' @param operation (expression as character string) expression to be computed on data
#' @param description (character)
#' @return NULL, side-effect: definition of S4 class derived from PreprocessorClass and corresponding transformdata-method
#' @details The user-defined S4 class definitions are stored in global environment and thus the function can not be used from an other package.
#'
#' scaleexample <- function(dataobject) dataobject <- initializedataclassobject(data.frame(x=scale(dataobject@@x), dataobject@@y))
#' setpreprocessor("scaleexample", "scaleexample(dataobject)")
#'
#' setpreprocessor() runs a test on test data frame and prints the outcome of the test.
#'
#' @export

setpreprocessor <- function(classname, operation, description){

  if (class(classname)!="character") {stop("Argument 'classname' must be a character vector")}
  if (class(operation)!="character") {stop("Argument 'operation' must be a character vector")}
  if (class(description)!="character") {stop("Argument 'description' must be a character vector")}

  # save operation to specific environment for function getpreprocessor()

  savedefinitiontoenvironment(classname, operation,description)

  # Create a subclass of PreprocessorClass

  setClass(classname, contains="PreprocessorClass", where=topenv(parent.frame()), prototype=prototype(objectname=classname, objectoperation=operation))

  # Define tranformdata method for the subclass

  setMethod("transformdata", where=topenv(parent.frame()), signature(object = classname), function(object, dataobject) {

    output <- eval(parse(text=operation))

    return(output)})

  # testing if preprocessor definition can be applied to random data

  paste("Test execution on Iris data successful:", testpreprocessors(classname, datasets::iris))

}

initializedataslot <- function(classname, dataobject){

  tryCatch({

    subclassobject <- new(classname)

    if (class(dataobject)=="DataClass") {transformeddata <- transformdata(subclassobject, dataobject)}

    if (class(dataobject)=="data.frame") {transformeddata <- transformdata(subclassobject, initializedataclassobject(dataobject))}

    if (is(dataobject, "PreprocessorClass")==TRUE) {transformeddata <- transformdata(subclassobject, dataobject@data)}

    subclassobject@data <- transformeddata

    return(subclassobject)

  }, error= function(e) return({subclassobject <- new(classname)})
  )

}


#' the MAIN function for interactive use.
#'
#' prepro() takes data, transforms it according to the given preprocessor and computes statistics of the
#' transformed data. The main use case is the chaining of the preprocessors as show in the examples below.

#' @param dataobject (sub class/ data frame/ DataClass) object
#' @param preprocessor (character) name of preprocessor
#' @param model (character) caret model name, note: the required model library must be attached, defaults to "rpart"
#' @param nholdout (integer) number of holdout rounds used in computation of classification accuracy, must be two or more, defaults to 2
#' @param cores (integer) number of cores used in parallel processing of holdout rounds, defaults to 1
#' @return object of PreprocessorClass sub class
#' @examples
#' ## a <- prepro(iris, "basicscale")
#' ## b <- prepro(a, "rfselect75")
#' ## d <- prepro(iris, "basicscale", "rf", nholdout=20, cores=2)
#' @details NOTE: If a data object has missing values, one of the imputation preprocessors must be applied first.
#' @export


prepro <- function(dataobject, preprocessor, model="rpart", nholdout=2, cores=1){

  # Argument validation

  ## data object validation
  validatedataobjectcall(dataobject)
  validatepreprocessorcall(as.list(preprocessor))

  # prepro

  doParallel::registerDoParallel(cores)

  subclassobject <- new(preprocessor)

  # Preprocess data depending on dataobject argument

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
  temp <- getprogrammaticprediction(temp, model, nholdout)
  subclassobject@classificationaccuracy <- apply(temp, 2, mean)[1]


  # Compute clustering tendency

  temp <- clustertend::hopkins(data@x, n=nrow(data@x)-1)
  subclassobject@hopkinsstatistic <- unname(unlist(temp))

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

#' get available preprocessors and their descriptions
#' @param type (character) default "description", alternatives "name" or "definition"
#' @param nro (integer) combination number
#' @export

getpreprocessors <- function(type="description", nro)   {

  preprocessornames <- as.list(ls(preprocessordefinitionstorage))

  if (type=="name") {
    output <- ls(preprocessordefinitionstorage)
  }

  if (type=="description"){
    temp1 <- paste("preprocessordefinitionstorage$", preprocessornames, "$description", sep="")
    preprocessordescriptions <- lapply(temp1, function(x) eval(parse(text=x)))
    output <- data.frame(name=as.character(preprocessornames), description=as.character(preprocessordescriptions))
  }

  if (type=="definition"){
  temp1 <- paste("preprocessordefinitionstorage$", preprocessornames, "$definition", sep="")
  preprocessordefinitions <- lapply(temp1, function(x) eval(parse(text=x)))
  output <- preprocessordefinitions[[nro]]
  }

return(output)

}
#' test preprocessing techniques against data
#'
#' Intended to be used when adding new preprocessing techniques with setpreprocessor().
#'
#' @param preprocessors (character) vector of preprocessors, by default gets all preprocessors with getpreprocessors()
#' @param data (data frame) to be tested against, defaults to random data frame without missing values
#' @return boolean, TRUE is all data object validations are TRUE
#' @examples
#' testpreprocessors()
#' @details testpreprocessor() with Iris as data argument is called from setpreprocessor()
#' @export
#' @keywords internal

testpreprocessors <- function(preprocessors=NULL, data=NULL){
  if (is.null(preprocessors)) {preprocessors <- getpreprocessors(type="name") }
  if (is.null(data)) {data <- data.frame(matrix(rbinom(4*30, 1, .5), ncol=4), class=sample(letters[1:2], 30, replace=TRUE))}
  cls <- as.list(preprocessors)
  testdata <- initializedataclassobject(data)
  temp <- lapply(cls, function(x) initializedataslot(x, testdata))
  temp1 <- lapply(temp, function(x) slot(x, "data"))
  return(reportexitstatus(temp1))
  }

savedefinitiontoenvironment <- function(classname, operation, description){

  definition <- as.character(eval(parse(text=paste("body(", gsub( "\\(.*$", "", operation ), ")", sep=""))))
  definition <- paste(definition, collapse="")

  value <- list(description=description, definition=definition)

  assign(classname, value, envir = preprocessordefinitionstorage)
}


validatedataobjectcall <- function(dataobject){
  isdf <- is.data.frame(dataobject)
  isdataclass <- is(dataobject, "DataClass")
  ispreprocessorclass <- is(dataobject, "PreprocessorClass")
  dataobjecttest <- any(c(isdf, isdataclass, ispreprocessorclass))
  if (dataobjecttest==FALSE){stop("Argument 'dataobject' must point to dataframe, DataClass or PreprocessorClass objects")}
}


