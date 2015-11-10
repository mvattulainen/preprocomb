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
#' @slot ORHskewness (numeric) skewness value of LOF scores
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

    # create expression
    if (mode=="numeric") {replacer <- "dataobject@x"} # only numeric columns from a DataClass object
    if (mode=="all") {replacer <- "dataobject"}
    functionexpression <- gsub("basedata", replacer, operation)

    # execute expression, can return either data frame of DataClass object
    output <- eval(parse(text=functionexpression))

    # numeric columns
    if (mode=="numeric") {newdataobject <- initializedataclassobject(data.frame(x=output, y=dataobject@y))}

    if (mode=="all") {newdataobject <- output}

    return(newdataobject)

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
#' ## a <- prepro(iris, "scale")
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

# DEFAULT PREPROCESSORS AND PHASES ==========================

# NO ACTION

setpreprocessor("noaction", "identity(basedata)")

# Low variance

setpreprocessor("nearzerovar", "nezevar(basedata)")

# Imputation

setpreprocessor("naomit", "na.omit(basedata)")
setpreprocessor("meanimpute", "data.frame(apply(basedata, 2, meanrep))")
setpreprocessor("knnimpute", "knnimputefunc(basedata)")
setpreprocessor("randomforestimpute", "rfimputefunc(basedata)", mode="all")

## Scaling
setpreprocessor("basicscale", "scale(basedata, center=FALSE)")
setpreprocessor("centerscale", "scale(basedata, center=TRUE)")
setpreprocessor("minmaxscale", "data.frame(apply(basedata, 2, range01))")
setpreprocessor("softmaxscale", "data.frame(apply(basedata, 2, DMwR::SoftMax))")

# Outlier removal
setpreprocessor("orhoutlier", "orhcut(basedata)", mode="all")

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
scaling <- setphase("scaling", c("noaction", "basicscale", "centerscale", "minmaxscale", "softmaxscale"), FALSE)
outliers <- setphase("outliers", c("noaction", "orhoutlier"), FALSE)
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
  temp <- lapply(cls, function(x) initializedataslot(x, testdata))
  temp1 <- lapply(temp, function(x) slot(x, "data"))
  print(reportexitstatus(temp1))
  return(temp1)
  }


