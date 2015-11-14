
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

## FUNCTIONS

# Mode for VOTE

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]}

# Test for even/odd

is.odd <- function(x) x %% 2 != 0

range01 <- function(y){
  newrange <- (y-min(y))/(max(y)-min(y))
}

meanimpute_aux <- function(x){
  x[is.na(x)] <- mean(x, na.rm=TRUE) #convert the item with NA to median value from the column
  x
  }

#globalVariables(c("result","combinationevaluation", "predictor", "skewness"))

