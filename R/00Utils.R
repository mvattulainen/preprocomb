
## NAMESPACE

#' @importFrom randomForest randomForest
NULL

#' @importFrom methods setClass setGeneric setMethod  extends getClass is new prototype signature slot
NULL

#' @import caret
NULL

#' @importFrom stats cor lowess predict quantile rbinom sd
NULL

#' @importFrom utils tail head
NULL

#'@import foreach
NULL

## FUNCTIONS

# Mode for VOTE

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]}

# Test for even/odd

is.odd <- function(x) x %% 2 != 0

# Extract validation results

extract <- function(x){
  row <- c(variance=x@variance, finite=x@finite, completeobs=x@completeobs, classbalance=x@classbalance, ntopratiotwoplus=x@ntopratiotwoplus, mindimensions=x@mindimensions)
}

# ENVIRONMENT FOR STORING PREPRECESSOR DEFINITIONS

#' environment for storing preprocessor definitions
#'
#' an environment to get the preprocessing technique function bodies
#' @export
preprocessordefinitionstorage <- new.env()


#globalVariables(c("result","combinationevaluation", "predictor", "skewness"))

#' preprocomb example
#'
#' An example grid object made of modified Iris-data with 3200 combinations \cr
#' evaluated with knn classifier classification accuracy and four holdrounds for each.
#'
#' @format A PreProCombClass object
"exampleresult"
