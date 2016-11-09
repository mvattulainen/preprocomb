
## NAMESPACE, IMPORTS

#' @importFrom randomForest randomForest
NULL

#' @importFrom methods setClass setGeneric setMethod  extends getClass is new prototype signature slot
NULL

#' @import caret
NULL

#' @import ggplot2
NULL

#' @importFrom stats cor lowess predict quantile rbinom sd
NULL

#' @importFrom utils tail head
NULL

#'@import foreach
NULL

#' @importFrom graphics boxplot
NULL

## SUPPORTING FUNCTIONS

# get mode

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]}

# test for even/odd

is.odd <- function(x) x %% 2 != 0

# extract validation results

extract <- function(x){
  row <- c(variance=x@variance, finite=x@finite, completeobs=x@completeobs, classbalance=x@classbalance, ntopratiotwoplus=x@ntopratiotwoplus, mindimensions=x@mindimensions)
}

## SUPPORTING ENVIRONMENT

#' environment for storing preprocessor definitions
#'
#' an environment to save and get the preprocessing technique function bodies. Note, this environment
#' is only created for function getpreprocessor(). \cr
#' @export
#' @keywords internal
preprocessordefinitionstorage <- new.env()

## SUPPORTING DATA

#' preprosim example
#'
#' examplesimulation <- preprosimrun(iris, fitmodels=FALSE) # 6561 contaminated data sets \cr
#' @format A PreprosimClass object
#' @keywords internal
"examplesimulation"

#' setgrid example
#'
#' Contaminated Iris-data preprocessed with 90 combinations. \cr
#' examplecombgrid <- setgrid(phases=c("imputation", "scaling", "smoothing"), data=contaminateddf) \cr
#' @format GridClass
"examplecombgrid"

#' preprocomb example
#'
#' Modified Iris-data preprocessed with 90 combinations and evaluated with \cr
#' svmRadial classifier and 400 times repreated holdout validation. \cr
#' @format ResultClass
"exampleresult"

#' metaheur example
#'
#' examplemetaheur <- metaheur(examplegrid, model="svmRadial", iterations = 30, nholdout = 400) \cr
#' @format A MetaheurClass object
#' @keywords internal
"examplemetaheur"

globalVariables(c("out", "Index", "value", "outlier"))

