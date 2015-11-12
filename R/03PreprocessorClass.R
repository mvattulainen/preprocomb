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
#' @slot ORHskewness (numeric) skewness value of ORH scores
#' @slot callhistory (character) vector of current and previous calls
#' @export

setClass("PreprocessorClass", representation(objectname="character", objectoperation="character", data="DataClass", classificationaccuracy="numeric", hopkinsstatistic="numeric", ORHskewness="numeric", callhistory="character"))

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
#' If an operation utilizes class labels or deletes rows from numeric columns, mode="all" must be
#' used and it must return a DataClass object.
#'
#' @param classname (character)
#' @param operation (expression as character string)
#' @return NULL, definition of S4 class derived from PreprocessorClass and corresponding transformdata-method
#' @examples
#' ## Set of examples using only numeric variables and no class labels
#' ## setpreprocessor("naomit", "na.omit(basedata)")
#' @details The user-defined S4 class definitions are stored in global environment and thus the
#' function can not be used from an other package.
#' @export

setpreprocessor <- function(classname, operation){

  setClass(classname, contains="PreprocessorClass", where=topenv(parent.frame()), prototype=prototype(objectname=classname, objectoperation=operation))

  setMethod("transformdata", where=topenv(parent.frame()), signature(object = classname), function(object, dataobject) {

    output <- eval(parse(text=operation))

    return(output)

  })

}



interactiveprediction <- function(object, predictor, nholdout){

  tryCatch({

    data <- object@data
    data <- data.frame(data@x, y=data@y)
    con <- numeric(nholdout)
    fitControl <- caret::trainControl(method = "boot", repeats=2)

    for (i in 1:nholdout){

      training <- caret::createDataPartition(data$y, times=1, list=FALSE, p=0.66)[,1]

      intrain <- data[training,]
      intest <- data[-training,]

      model <- caret::train(y ~., data=intrain, method=predictor, trControl = fitControl)
      prediction <- as.data.frame(predict(model, newdata=intest))
      con[i] <- mean(as.character(prediction[,1])==as.character(intest$y))

    }

    con <- mean(con)

  }, error= function(e) return(NA) )

}

#' prepro
#'
#' prepro is the main function for interactive use. It takes data, transforms it according to the given
#' preprocessor and computes statistics of the transformed data. The main use case is the chaining of
#' the preprocessors as show in the examples below.
#'
#' @param classname (character) name of preprocessor (i.e. PreprocessorClass sub class as defined by setpreprocessor())
#' @param dataobject (sub class/ data frame/ DataClass) object
#' @param model (character) caret model name, note: the required model library must be attached, defaults to "knn"
#' @param nholdout (integer) number of holdout rounds used in computation of classification accuracy, must be two or more, defaults to two
#' @param nsharehopkins (integer) denominator for sample size for hopkins statistics, defauls to three  (n=nrow(data)/3)
#' @return object of PreprocessorClass sub class
#' @examples
#' ## a <- prepro(iris, "basicscale")
#' ## b <- prepro(a, "rfselect75")
#' ## d <- prepro(iris, "scale", "rf", 20, 2, 10)
#' @export

prepro <- function(dataobject, classname, model="knn", nholdout=2, nsharehopkins=3){

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

  subclassobject@data <- validatedata(transformeddata)

  subclassobject@classificationaccuracy <- suppressWarnings(interactiveprediction(subclassobject, predictor, nholdout))

  subclassobject@hopkinsstatistic <- unname(unlist(clustertend::hopkins((subclassobject@data)@x, n=as.integer(nrow((subclassobject@data)@x)/nsharehopkins)   )))

  orh_score <- suppressMessages(DMwR::outliers.ranking((subclassobject@data)@x))
  orh_rank <- orh_score$prob.outliers[orh_score$rank.outliers]
  subclassobject@ORHskewness <- e1071::skewness(orh_rank)

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
  cat("# not multicollinear above .9:", object@data@corrbelowdotnine, "\n")
  cat("# n to p ratio more than 2:", object@data@ntopratiotwoplus, "\n")
  cat("# 3 or more predictors and more than 20 observations:", object@data@mindimensions, "\n")
  } )



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
  temp <- lapply(cls, function(x) initializedataslot(x, testdata))
  temp1 <- lapply(temp, function(x) slot(x, "data"))
  print(reportexitstatus(temp1))
  return(temp1)
  }


