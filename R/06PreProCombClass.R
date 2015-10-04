#' @include 05PredictionControlClass.R
NULL

# setOldClass("C5.0")

#' PreProCombClass
#'
#' PreProCombClass represents the analysis of preprocessing combinations
#' @slot best (data frame) best (that is, lowest misclassification error) combination
#' @slot all (data frame) all preprocessing combinations and respective misclassification errors
#' @slot rules assosiation rules for "low" (that is, lowest 10 percent) misclassification error

setClass("PreProCombClass", representation(rawall="data.frame", rawcat="data.frame", allce="data.frame", bestce="data.frame", allclustering="data.frame", bestclustering="data.frame", alloutliers="data.frame"))

#' preprocomb
#'
#' preprocomb outputs the analysis of preprocessing combinations
#
#' @param predictors (character) vector of predictors (names of models as defined in package caret)
#' @param grid (GridClass) object
#' @param nholdout (integer) number of holdout rounds, defaults to 1
#' @param search (character) defaults to "exhaustive" full blind search, "random" search 20 percent of grid, "grid" grid search 10 percent
#' @examples
#' ## modifiediris <- droplevels(iris[-c(1:60),])
#' ## grid <- setgrid(phases=c("outlier", "scaling", "sampling"), data=modifiediris)
#' ## result <- preprocomb(predictors=c("svmRadial"), grid=grid, nholdout=2, search="grid")
#' ## result@@all
#' ## result@@rules
#' @export

preprocomb <- function(predictors, grid, nholdout=1, search="exhaustive"){

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
  cutpoint <- quantile(tempout[,(ncol(tempout)-2)], .10)
  tempout$target <- cut(tempout[,ncol(tempout)], breaks=c(-Inf, cutpoint, Inf), labels=c("low", "high"))
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

