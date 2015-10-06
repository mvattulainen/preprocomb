#' @include 05PredictionControlClass.R
NULL

# setOldClass("C5.0")

setClass("PreProCombClass", representation(rawall="data.frame", rawcat="data.frame", allce="data.frame", bestce="data.frame", allclustering="data.frame", bestclustering="data.frame", alloutliers="data.frame"))

#' preprocomb
#'
#' preprocomb the function of programmatic mode. It executes the computation of
#' misclassification errors, hopkins statistic and ORH outlier scores. The result
#' PreProCombClass object is used either to access its slots or to plot it with
#' preproplot()
#
#' @param predictors (character) vector of predictors (names of models as defined in package caret), there must be an odd number of models
#' @param grid (GridClass) object representing the grid of combinations
#' @param nholdout (integer) number of holdout rounds, must be two or more, defaults to 2
#' @param search (character) defaults to "exhaustive" full blind search, "random" search 20 percent of grid, "grid" grid search 10 percent
#' @examples
#' ## modifiediris <- droplevels(iris[-c(1:60),])
#' ## grid <- setgrid(phases=c("outlier", "scaling"), data=modifiediris)
#' ## result <- preprocomb(predictors=c("svmRadial"), grid=grid, nholdout=2, search="exhaustive")
#' ## result@@allce
#' ## result@@allclustering
#' ## result@@alloutliers
#' ##
#' ## newphases <- c("outlier", "smoothing", "scaling", "selection", "sampling")
#' ## newpredictors <- c("knn", "rf", "svmRadial")
#' ## grid1 <- setgrid(phases=newphases, data=modifiediris)
#' ## result1 <- preprocomb(predictors=newpredictors, grid=grid1, nholdout=2, search="grid")
#' @export

preprocomb <- function(predictors, grid, nholdout=2, search="exhaustive"){

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
  preprocombclassobject@allce <- data.frame(cbind(out[,1:nphase], preproout))

  # best combination

  best <- head(order(prepromeans[,ncol(prepromeans)]))
  preprocombclassobject@bestce <- preprocombclassobject@allce[best,]

  # rawdata

  rawall <- out
  st1 <- paste(c(predictors, "VOTE"), "Mean", sep="")
  st2 <- paste(c(predictors, "VOTE"), "SD", sep="")
  colnames(rawall)[(nphase+1):(ncol(rawall)-2)] <- c(st1, st2)
  preprocombclassobject@rawall <- rawall

  # rawcat

  tempout <- preprocombclassobject@rawall
  cutpoint <- quantile(tempout[,voteposition], .20)
  tempout$target <- cut(tempout[,voteposition], breaks=c(-Inf, cutpoint, Inf), labels=c("low", "high"))
  tempout <- tempout[, -c((nphase+1):(ncol(tempout)-1))]
  preprocombclassobject@rawcat <- tempout

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

