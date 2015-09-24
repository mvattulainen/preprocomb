
## NAMESPACE

#' @importFrom randomForest randomForest
NULL

#' @importFrom methods setClass setGeneric setMethod
NULL

#' @import arules
NULL

#' @import caret
NULL

## UTILS

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]}

range01 <- function(y){
  newrange <- (y-min(y))/(max(y)-min(y))
}

meanrep <- function(x){
  x[is.na(x)] =mean(x, na.rm=TRUE)
  x
}

is.odd <- function(x) x %% 2 != 0

lofcut <- function(x){
lof_score <- DMwR::lofactor(x, k=5)
lof_score[is.finite(lof_score)==FALSE] <- 0 #dirty fix
lof_cut <- quantile(lof_score, .95)
lof_obs <- which(lof_score > lof_cut)
x <- x[-lof_obs,]
}

orhcut <- function(x){
orh_score <- suppressMessages(DMwR::outliers.ranking(x))
orh_rank <- orh_score$prob.outliers[orh_score$rank.outliers]
orh_cut <- quantile(orh_rank, .95)
orh_obs <- which(orh_rank >= orh_cut)
x <- x[-orh_obs,]
}

oversample <- function(dataobject){

    freq <- table(dataobject@y)
    freq <- as.integer(freq) # number of observations in each factor level
    freqcum <- cumsum(freq) # start of each factor level in sequence of observations
    freqorder <- order(freq) # smallest factor level first
    seqend <- freqcum[freqorder[1]] # end of smallest factor level in sequence of observations
    seqbegin <- seqend - freq[freqorder[1]] # end minus length of smallest factor level
    samplesize <- freq[freqorder[2]]-freq[freqorder[1]] # different in size: smallest and second smallest
    extrasample <- sample(seqbegin:seqend, samplesize, replace=TRUE)

    newx <- data.frame(rbind(dataobject@x, dataobject@x[extrasample,]))
    newy <- factor(c(as.character(dataobject@y), as.character(dataobject@y[extrasample])))

    dataobject@x <- newx
    dataobject@y <- newy
    return(dataobject)
}

rfimputefunc <- function(dataobject){
  if (any(is.na(dataobject@x))){
  res <- randomForest::rfImpute(dataobject@y ~ ., dataobject@x)
  dataobject@x <- res[,2:ncol(res)]
  dataobject@y <- res[,1]
  }
  return(dataobject)
}

knnimputefunc <- function(x){
  if (any(is.na(x))){
    x <- DMwR::knnImputation(x, k=5)
      }
  return(x)
}


rfimportance <- function(dataobject, qt){
  rf.imp <- randomForest::randomForest(dataobject@y ~ ., data=dataobject@x, ntree=100, keep.forest=FALSE, importance=TRUE)
  temp <- data.frame(randomForest::importance(rf.imp))
  temp1 <- temp$MeanDecreaseAccuracy > quantile(temp$MeanDecreaseAccuracy, qt)
  dataobject@x  <- dataobject@x[,temp1]
  return(dataobject)
}

globalVariables(c("result"))

#testthat::expect_equal(testpreprocessors(), "Exit status: OK: Stable computation of misclassification errors expected.")
