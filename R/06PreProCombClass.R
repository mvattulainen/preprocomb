#' @include 05PredictionControlClass.R
NULL

# setOldClass("C5.0")



#' container for combination evaluation
#'
#' This class implements the separation of data used for
#' analysis and analysis of the data. The latter can include
#' computation of association rules as in showrules().
#'
#' @slot rawall (data frame) all results
#' @slot catclassification (data frame) classification accuracy categorized, "high" is more than 80 percent quantile value
#' @slot allclassification (data frame) classification accuracy, means and standard deviations
#' @slot bestclassification (data frame) best classification accuracy combinations
#' @slot allclustering (data frame) hopkins statistics values
#' @slot bestclustering (data frame) best hopkins statistics combinations
#' @slot alloutliers (data frame) ORH outlier score for 95 percent quantile value
#' @slot walltime (integer) execution time in minutes by wall time (not computation time)
#' @export

setClass("PreProCombClass", representation(rawall="data.frame", catclassification="data.frame", allclassification="data.frame", bestclassification="data.frame", allclustering="data.frame", bestclustering="data.frame", alloutliers="data.frame", walltime="integer"))

#' the MAIN function of programmatic use.
#'
#' preprocomb executes the computation of classification accuracy, hopkins statistic and ORH outlier score.
#' An alternative to preprocomb is to use package 'metaheur' for faster finding of near-optimal combinations.
#
#' @param models (character) vector of models (names of models as defined in package caret), defaults to "rpart"
#' @param gridclassobject (GridClass) object representing the grid of combinations
#' @param nholdout (integer) number of holdout rounds for predictive classification, must be two or more, defaults to two
#' @param searchmethod (character) defaults to "exhaustive" full blind search, "random" search 20 percent of grid, "grid" grid search 10 percent
#' @param predict (boolean) compute predictions, defaults to TRUE
#' @param cluster (boolean) compute clustering tendency, defaults to FALSE
#' @param outlier (boolean) compute outlier tendency, defaults to FALSE
#' @param cores (integer) number of cores used in parallel processing of holdout rounds, defaults to 1
#' @return a PreProCombClass object
#' @details caret messages will be displayed during processing
#' @examples
#' ## modifiediris <- droplevels(iris[-c(1:60),])
#' ## grid <- setgrid(phases=c("outliers", "scaling"), data=modifiediris)
#' ## library(kernlab)
#' ## result <- preprocomb(models=c("svmRadial"), grid=grid, nholdout=1, search="grid")
#' ## result@@allclassification
#' ## result@@allclustering
#' ## result@@alloutliers
#' ## result@@rawall
#' ## result@@catclassification
#' ##
#' ## newphases <- c("outliers", "smoothing", "scaling", "selection", "sampling")
#' ## newmodels <- c("knn", "rf", "svmRadial")
#' ## grid1 <- setgrid(phases=newphases, data=modifiediris)
#' ## result1 <- preprocomb(models=newmodels, grid=grid1, nholdout=1, search="grid")
#' @export

preprocomb <- function(models="rpart", gridclassobject, nholdout=2, searchmethod="exhaustive", predict=TRUE, cluster=FALSE, outlier=FALSE, cores=1){

  ## PREPARING ACTIVITIES

  doParallel::registerDoParallel(cores=cores)

  starttime <- Sys.time()

  predictors <- models

  if(class(predictors)!="character"){stop("The argument predictors must a character vector.")}

  if(class(gridclassobject)!="GridClass"){stop("The argument grid must be a GridClass object.")}

  gridsearchtest <- nrow(gridclassobject@grid) < 11 & searchmethod %in% c("grid", "random")

  if(gridsearchtest==TRUE) {stop("There must be more than 10 combinations to use grid or random search.")}

  supportedsearches <- c("exhaustive", "random", "grid")
  if (!searchmethod %in% supportedsearches) {stop("Argument 'search' must on one of the following: 'exhaustive', 'random', 'grid")}

  if (class(predict)!="logical"){stop("Argument 'predict' must a logical")}
  if (class(cluster)!="logical"){stop("Argument 'cluster' must a logical")}
  if (class(outlier)!="logical"){stop("Argument 'outlier' must a logical")}

  ## CORE FUNCTIONALITY

  out <- combinationevaluation(predictors, gridclassobject, nholdout, searchmethod, predict, cluster, outlier)

  preprocombclassobject <- new("PreProCombClass")

  ## FININALIZE THE RESULTS

  # all classification accuracies

  preproout <- data.frame(matrix(paste(format(round(as.matrix(out[[1]]),2),nsmall=2), format(round(as.matrix(out[[2]]),2),nsmall=2), sep="+-"), nrow=nrow(out[[1]])))
  colnames(preproout) <- c(predictors, "ALL_MEAN")
  preprocombclassobject@allclassification <- data.frame(as.data.frame(out[[5]]), preproout)

  # best best classification accuracies

  bestaccuracies <-  head(preprocombclassobject@allclassification[order(preprocombclassobject@allclassification$ALL_MEAN, decreasing=TRUE),])
  preprocombclassobject@bestclassification <- bestaccuracies

  # raw data

  rawall <- data.frame(out[[5]], out[[1]], out[[2]], out[[3]], out[[4]])

  colnames(rawall)[(ncol(out[[5]])+1):ncol(rawall)] <- c(paste(c(predictors, "ALL_MEAN"), "Mean", sep=""), paste(c(predictors, "ALL_MEAN"), "SD", sep=""), "Hopkins", "Orh_skewness")
  preprocombclassobject@rawall <- rawall

  # raw categorical

  if (predict==TRUE){
  tempout <- data.frame(out[[5]], out[[1]][ncol(out[[1]])])
  colnames(tempout)[ncol(tempout)] <- "ALL_MEAN"
  cutpoint <- quantile(tempout$ALL_MEAN, .80)
  tempout$target <- cut(tempout$ALL_MEAN, breaks=c(-Inf, cutpoint, Inf), labels=c("low", "high"))
  tempout <- tempout[,-(ncol(tempout)-1)]
  preprocombclassobject@catclassification <- tempout
  }

  # by clustering tendency

  preprocombclassobject@allclustering <- data.frame(out[[5]], hopkins=round(out[[3]],2))

  bestclustering <- head(preprocombclassobject@allclustering[order(preprocombclassobject@allclustering$hopkins, decreasing=TRUE),])
  preprocombclassobject@bestclustering <- bestclustering

  # by outliers

  preprocombclassobject@alloutliers <- data.frame(out[[5]], Orh_skewness=round(out[[4]],2))

  doParallel::stopImplicitCluster()

  endtime <- Sys.time()

  preprocombclassobject@walltime <- as.integer(difftime(endtime, starttime, units="mins"))

  return(preprocombclassobject)
}

