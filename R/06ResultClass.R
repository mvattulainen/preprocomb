#' @include 05PredictionControlClass.R
NULL

#' An S4 class representing preprocessing combination evaluation results
#'
#' This class implements the separation of data used for
#' analysis and analysis of the data. The latter can include
#' computation of association rules as in showrules().
#'
#' @slot rawall (data frame) all results
#' @slot catclassification (data frame) classification accuracy categorized, "high" is more than 80 percent quantile value
#' @slot allclassification (data frame) classification accuracy, means and standard deviations
#' @slot bestclassification (data frame) best classification accuracy combinations
#' @slot worstclassification (data frame) worst classification accuracy combinations
#' @slot allclustering (data frame) hopkins statistics values
#' @slot bestclustering (data frame) best hopkins statistics combinations
#' @slot walltime (integer) execution time in minutes by wall time (not computation time)
#' @export

setClass("ResultClass", representation(rawall="data.frame", catclassification="data.frame", allclassification="data.frame", bestclassification="data.frame", worstclassification="data.frame", allclustering="data.frame", bestclustering="data.frame", walltime="integer"))

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
#' @param cores (integer) number of cores used in parallel processing of holdout rounds, defaults to 1
#' @return a ResultClass object
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

preprocomb <- function(models="rpart", gridclassobject, nholdout=2, searchmethod="exhaustive", predict=TRUE, cluster=FALSE, cores=1){

  ## PREPARING ACTIVITIES

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

  doParallel::registerDoParallel(cores=cores)
  rawoutput <- combinationevaluation(predictors, gridclassobject, nholdout, searchmethod, predict, cluster)
  doParallel::stopImplicitCluster()

  ## FININALIZE THE RESULTS

  resultclassobject <- new("ResultClass")

  resultclassobject@rawall <- formatrawdata(rawoutput, predictors)

  if (predict==TRUE){
    resultclassobject@catclassification <- formatcategoricaldata(rawoutput)
    resultclassobject@allclassification <- formatclassificationaccuracy(rawoutput, predictors)
    resultclassobject@bestclassification <- formatorderclassificationaccuracy(resultclassobject, type="decreasing")
    resultclassobject@worstclassification <- formatorderclassificationaccuracy(resultclassobject, type="increasing")
  }

  if (cluster==TRUE){
  resultclassobject@allclustering <- formatallclustering(rawoutput)
  resultclassobject@bestclustering <- formatbestclustering(resultclassobject)
  }

  endtime <- Sys.time()

  resultclassobject@walltime <- as.integer(difftime(endtime, starttime, units="mins"))

  return(resultclassobject)
}

## FORMATTING OF RAW OUTPUT

formatclassificationaccuracy <- function(out, predictors){
  preproout <- data.frame(matrix(paste(format(round(as.matrix(out[[1]]),2),nsmall=2), format(round(as.matrix(out[[2]]),2),nsmall=2), sep="+-"), nrow=nrow(out[[1]])))
  colnames(preproout) <- c(predictors, "ALL_MEAN")
  data.frame(as.data.frame(out[[5]]), preproout)
}

formatorderclassificationaccuracy <- function(resultclassobject, type){
  if (type=="decreasing"){
    output <-  head(resultclassobject@allclassification[order(resultclassobject@allclassification$ALL_MEAN, decreasing=TRUE),])
  }

  if (type=="increasing"){
    output <-  head(resultclassobject@allclassification[order(resultclassobject@allclassification$ALL_MEAN, decreasing=FALSE),])
  }

  return(output)
}

formatrawdata <- function(out, predictors){
rawall <- data.frame(out[[5]], out[[1]], out[[2]], out[[3]], out[[4]])
colnames(rawall)[(ncol(out[[5]])+1):ncol(rawall)] <- c(paste(c(predictors, "ALL_MEAN"), "Mean", sep=""), paste(c(predictors, "ALL_MEAN"), "SD", sep=""), "Hopkins", "Orh_skewness")
return(rawall)
}

formatcategoricaldata <- function(out){
  tempout <- data.frame(out[[5]], out[[1]][ncol(out[[1]])])
  colnames(tempout)[ncol(tempout)] <- "ALL_MEAN"
  cutpoint <- quantile(tempout$ALL_MEAN, .80)
  tempout$target <- cut(tempout$ALL_MEAN, breaks=c(-Inf, cutpoint, Inf), labels=c("low", "high"))
  tempout <- tempout[,-(ncol(tempout)-1)]
}

formatallclustering <- function(out){
  data.frame(out[[5]], hopkins=round(out[[3]],2))
}

formatbestclustering <- function(resultclassobject){
  bestclustering <- head(resultclassobject@allclustering[order(resultclassobject@allclustering$hopkins, decreasing=TRUE),])
}

