#' @include 05PredictionControl.R
NULL

setOldClass("C5.0")

#' PreProCombClass
#'
#' PreProCombClass represents the analysis of preprocessing combinations
#' @slot best (data frame) best (that is, lowest misclassification error) combination
#' @slot all (data frame) all preprocessing combinations and respective misclassification errors
#' @slot importance (data frame) variable importance of phases in predicting misclassification error
#' @slot rules assosiation rules for "low" (that is, last 10 percent) misclassification error

setClass("PreProCombClass", representation(best="data.frame", all="data.frame", importance="data.frame", tree="C5.0", rules="data.frame"))

#' preprocomb
#'
#' preprocomb outputs the analysis of preprocessing combinations
#
#' @param predictors (character) vector of predictors (names of models as defined in package caret)
#' @param grid (GridClass) object
#' @param nholdout (integer) number of holdout rounds, defaults to 1
#' @param search (character) defaults to "exhaustive" full blind search, "random" search 20 percent of grid, "grid" grid search 10 percent
#' @examples
#' ## grid <- setgrid(phases=c("outlier", "selection", "scaling"), data=iris)
#' ## result <- preprocomb(predictors=c('rf'), grid=grid, nholdout=1, search="exhaustive")
#' ## result@@best
#' ## result@@all
#' ## result@@tree
#' @export

preprocomb <- function(predictors, grid, nholdout=1, search="exhaustive"){

  predictioncontrolclassobject <- initializepredictioncontrolclassobject(predictors, grid)

  out <- combpredict(predictioncontrolclassobject, nholdout, search)

  preprocombclassobject <- new("PreProCombClass")

  tempfactor <- sum(sapply(out, is.factor)) # number of phases
  tempseq <- c(seq(1,tempfactor, 1), ncol(out))

  # best combination
  preprocombclassobject@best <- out[which.min(out[,ncol(out)]), tempseq] # BUG

  # all combinations
  preprocombclassobject@all <- out

  # variable importance

  tempout <- out[,tempseq]
  cutpoint <- quantile(tempout[,ncol(tempout)], .10)
  tempout$target <- cut(tempout[,ncol(tempout)], breaks=c(-Inf, cutpoint, Inf), labels=c("low", "high"))
  tempout <- tempout[, -length(tempseq)]
  out.rf <- randomForest(target ~ ., data=tempout, ntree=1000, keep.forest=FALSE, importance=TRUE)
  temp1 <- data.frame(randomForest::importance(out.rf)[,3])
  temp1$phase <- names(randomForest::importance(out.rf)[,3])
  rownames(temp1) <- NULL
  preprocombclassobject@importance <- temp1

  ## C5.0 Tree

  mod1 <- C50::C5.0(target ~ ., data = tempout)
  preprocombclassobject@tree <- mod1

  ##
  temp2 <- data.frame(lapply(tempout, factor))
  trans3 <- as(temp2, "transactions")
  rules <- apriori(trans3, parameter = list(support = 0.01, confidence = 0.8), appearance=list(rhs='target=low', default='lhs'))
  rout <-  as(rules, "data.frame")
  preprocombclassobject@rules <- rout

  return(preprocombclassobject)
}

bestcomb <- function(x) {result <- x@best}
allcombs <- function(x) {result <- x@all}
plotcomb <- function(x) {plot(result@tree)}
rulescomb <- function(x) {result <- x@rules}
