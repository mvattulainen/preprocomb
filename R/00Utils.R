
## NAMESPACE

#' @importFrom randomForest randomForest
NULL

#' @importFrom methods setClass setGeneric setMethod
NULL

#' @import arules
NULL

#' @import caret
NULL

#' @import C50
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

    data <- data.frame(dataobject@x, y=dataobject@y)

    if (nlevels(data$y) > 2) {stop("Oversampling can only be applied to binary class.")}

    freq <- table(data$y)
    temp <- order(freq)
    nsample <- freq[temp[2]]-freq[temp[1]]
    indexes <- which(data$y==names(freq)[temp[1]])
    tempsample <- sample(indexes, nsample, replace=TRUE)
    newdata <- initializedataclassobject(data.frame(rbind(data, data[tempsample,])))
    return(newdata)
}

undersample <- function(dataobject){

  data <- data.frame(dataobject@x, y=dataobject@y)

  if (nlevels(data$y) > 2) {stop("Undersampling can only be applied to binary class.")}

  freq <- table(data$y)
  temp <- order(freq)
  nsample <- freq[temp[1]]
  indexes <- which(data$y==names(freq)[temp[2]])
  indexes2 <- which(data$y==names(freq)[temp[1]])
  tempsample <- sample(indexes, nsample)
  finalsample <- c(indexes2, tempsample)
  newdata <- initializedataclassobject(data[finalsample,])
  return(newdata)
}

smotesample <- function(dataobject){
  temp <- data.frame(dataobject@x, y=dataobject@y)

  if (nlevels(temp$y) > 2) {stop("SMOTE can only be applied to binary class.")}

  temp1 <- as.numeric(table(temp$y))
  temp2 <- order(temp1)
  temp3 <- temp1[temp2[2]]/temp1[temp2[1]]
  temp4 <- temp1[temp2[2]]/(temp1[temp2[2]]-temp1[temp2[1]])
  newData <- DMwR::SMOTE(y ~ ., temp, perc.over = 100*temp3, perc.under=100*temp4)
  dataobject <- initializedataclassobject(newData)
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

nzv <- function(x)
{
  temp <- caret::nearZeroVar(x)
  if (length(temp) !=0) {
    x <- x[,-temp]}
  return(x)
}

smoothlowess <- function(y){
  result <-data.frame(round(apply(y, 2, function(y) lowess(y[order(y)], f=1/2)[[2]][match(y, y[order(y)])]),2))
}



globalVariables(c("result","combpredict", "predictor", "skewness"))

#testthat::expect_equal(testpreprocessors(), "Exit status: OK: Stable computation of misclassification errors expected.")
