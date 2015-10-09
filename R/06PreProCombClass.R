#' @include 05PredictionControlClass.R
NULL

# setOldClass("C5.0")

#' PreProCombClass
#'
#' PreproCombClass is a container for output of executing preprocomb function.
#' It is an interface for extension of the system.
#' @slot rawall (data frame) all results
#' @slot catclassification (data frame) classification accuracy categorized, "high" is more than 80 percent quantile value
#' @slot allclassification (data frame) classification accuracy, means and standard deviations
#' @slot bestclassification (data frame) best classification accuracy combinations
#' @slot allclustering (data frame) hopkins statistics values
#' @slot bestclustering (data frame) best hopkins statistics combinations
#' @slot alloutliers (data frame) ORH outlier score for 95 percent quantile value
#' @export

setClass("PreProCombClass", representation(rawall="data.frame", catclassification="data.frame", allclassification="data.frame", bestclassification="data.frame", allclustering="data.frame", bestclustering="data.frame", alloutliers="data.frame"))

#' preprocomb
#'
#' preprocomb the function of programmatic mode. It executes the computation of
#' classification accuracy, hopkins statistic and ORH outlier score. See also
#' PreProCombClass.
#
#' @param models (character) vector of models (names of models as defined in package caret), there must be an odd number of models
#' @param grid (GridClass) object representing the grid of combinations
#' @param nholdout (integer) number of holdout rounds, must be two or more, defaults to two
#' @param search (character) defaults to "exhaustive" full blind search, "random" search 20 percent of grid, "grid" grid search 10 percent
#' @return a PreProCombClass object
#' @details caret messages will be displayed during processing
#' @examples
#' ## modifiediris <- droplevels(iris[-c(1:60),])
#' ## grid <- setgrid(phases=c("outlier", "scaling"), data=modifiediris)
#' ## library(kernlab)
#' ## result <- preprocomb(models=c("svmRadial"), grid=grid, nholdout=2, search="exhaustive")
#' ## result@@allclassification
#' ## result@@allclustering
#' ## result@@alloutliers
#' ## result@@rawall
#' ## result@@catclassification
#' ##
#' ## newphases <- c("outlier", "smoothing", "scaling", "selection", "sampling")
#' ## newmodels <- c("knn", "rf", "svmRadial")
#' ## grid1 <- setgrid(phases=newphases, data=modifiediris)
#' ## result1 <- preprocomb(models=newmodels, grid=grid1, nholdout=2, search="grid")
#' @export

preprocomb <- function(models, grid, nholdout=2, search="exhaustive"){

  predictors <- models

  predictioncontrolclassobject <- initializepredictioncontrolclassobject(predictors, grid)

  out <- combpredict(predictioncontrolclassobject, nholdout, search)

  preprocombclassobject <- new("PreProCombClass")

  # some values for subsetting
  nphase <- ncol(predictioncontrolclassobject@grid@grid)
  voteposition <- nphase + length(predictors) + 1
  tempseq <- c(seq(1,nphase), voteposition)

  # all combinations

  prepromeans <- as.matrix(out[,(nphase+1):(nphase+1+length(predictors))])
  preprosds <- as.matrix(out[,(nphase+1+length(predictors)+1):(ncol(out)-2)])
  preproout <- data.frame(matrix(paste(prepromeans, preprosds, sep="+-"), nrow=nrow(prepromeans)))
  colnames(preproout) <- c(predictors, "VOTE")
  preprocombclassobject@allclassification <- data.frame(cbind(out[,1:nphase], preproout))

  # best combination

  best <- tail(order(prepromeans[,ncol(prepromeans)]))
  preprocombclassobject@bestclassification <- preprocombclassobject@allclassification[best,]

  # rawdata

  rawall <- out
  st1 <- paste(c(predictors, "VOTE"), "Mean", sep="")
  st2 <- paste(c(predictors, "VOTE"), "SD", sep="")
  colnames(rawall)[(nphase+1):(ncol(rawall)-2)] <- c(st1, st2)
  preprocombclassobject@rawall <- rawall

  # rawcat

  tempout <- preprocombclassobject@rawall
  cutpoint <- quantile(tempout[,voteposition], .80)
  tempout$target <- cut(tempout[,voteposition], breaks=c(-Inf, cutpoint, Inf), labels=c("low", "high"))
  tempout <- tempout[, -c((nphase+1):(ncol(tempout)-1))]
  preprocombclassobject@catclassification <- tempout

  # by clustering tendency

  clseq <- c(seq(1,nphase), (ncol(out)-1))

  preprocombclassobject@allclustering <- out[,clseq]

  bestclustering <- tail(order(preprocombclassobject@allclustering[,ncol(preprocombclassobject@allclustering)]))
  preprocombclassobject@bestclustering <- preprocombclassobject@allclustering[bestclustering,]

  # by outliers

  outseq <- c(seq(1,nphase), ncol(out))

  preprocombclassobject@alloutliers <- out[,outseq]


  return(preprocombclassobject)
}

