
## NAMESPACE, IMPORTS

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
preprocessordefinitionstorage <- new.env()

## SUPPORTING DATA

#' preprocomb example
#'
#' Modified Iris-data preprocessed with 540 combinations and evaluated with \cr
#' support vector machine classifier. \cr
#'
#' # testdata \cr
#' set.seed(1) \cr
#' testdata <- iris \cr
#' testdata[sample(1:150,40),3] <- NA # add missing values to the third variable \cr
#' testdata[,4] <- rnorm(150, testdata[,4], 2) # add noise to the fourth variable \cr
#' testdata$Irrelevant <- runif(150, 0, 1) # add an irrelevant feature \cr
#'
#' # grid with five phases totalling 540 combinations \cr
#' examplegrid <- setgrid(phases=c("imputation", "outliers", "scaling", "smoothing", "selection"), data=testdata)
#'
#' # evaluation of the grid \cr
#' exampleresult <- preprocomb(grid=examplegrid, models=c("svmRadial"), nholdout=10, cluster=TRUE, outlier=TRUE, cores=2)
#'
#' @format A PreProCombClass object
"exampleresult"
