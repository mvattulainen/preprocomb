#' @include 04GridClass.R
NULL


# setClass("PredictionClass", representation(output="data.frame"))

setClass("PredictionControl", representation(predictors="character", grid="GridClass"))

initializepredictioncontrolclassobject <- function(predictors, grid)
{
  if(class(predictors)!="character"){stop("The argument predictors must a character vector.")}
  if(is.odd(length(predictors))!=TRUE){stop("The number of predictors must be an even number.")}
  if(class(grid)!="GridClass"){stop("The argument grid must be a GridClass object.")}

  predictioncontrolclassobject <- new("PredictionControl")
  predictioncontrolclassobject@predictors <- predictors
  predictioncontrolclassobject@grid <- grid
return(predictioncontrolclassobject)
}


# Get misclassification error/ programmatic mode/ many models

getcombprediction <- function(dat1, predictors, fitControl){

  training <- caret::createDataPartition(dat1$y, times=1, list=FALSE, p=0.66)[,1] ## NOTE: <=3

  intrain <- dat1[training,]
  intest <- dat1[-training,]

  model_list <- caretEnsemble::caretList(y ~., data=intrain, methodList=predictors, trControl=fitControl)
  prediction <- as.data.frame(predict(model_list, newdata=intest))
  prediction$vote <- apply(prediction, 1, Mode)
  output <- as.numeric(lapply(prediction, function(x) mean(as.character(x)==as.character(intest$y))))

}

gethopkins <- function(dat){
output <- round(unlist(clustertend::hopkins(dat@x, n=as.integer(nrow(dat@x)/3))),2)
}

getorh <- function(dat){
  orh_score <- suppressMessages(DMwR::outliers.ranking(dat@x))
  orh_rank <- orh_score$prob.outliers[orh_score$rank.outliers]
  output <- round(quantile(orh_rank, .95),2)
}

## PREDICTION ================================================

combpredict <- function(predictioncontrol, nholdout, search){

  # initializations
  grid <- predictioncontrol@grid
  predictors <- predictioncontrol@predictors
  fitControl <- caret::trainControl(method="boot", repeats=1)


  # Optimization method
  if (search=="exhaustive") {preproseq <- seq(1, nrow(grid@grid), 1)}
  if (search=="random") {preproseq <- sample(1:nrow(grid@grid), as.integer(nrow(grid@grid)/5))}
  if (search=="grid") {
  preproseq <- as.list(seq(1, nrow(grid@grid), by=as.integer(nrow(grid@grid)/(nrow(grid@grid)/10))))
  preproseq <- unlist(lapply(preproseq, function(x) x+sample(0:2, 1)))
  }

  charactergrid <- apply(grid@grid[preproseq,], 2, as.character)
  ncomputations <- 2*(length(predictors)+1)

  out <- data.frame(matrix(nrow=length(preproseq), ncol=ncomputations))
  cltend <- numeric(length(preproseq))
  orhquantile <- numeric(length(preproseq))

  # for each selected row in the grid

  for (j in preproseq)
  {
    dat <- grid@data[[j]]
    dat1 <- data.frame(y=dat@y, x=dat@x)

    temp <- data.frame(matrix(nrow=nholdout, ncol=length(predictors)+1))

    # fit models defined in predictors and validate with nholdout times repeated holdout method

    for (i in 1:nholdout){

      temp[i,] <- getcombprediction(dat1, predictors, fitControl)

    }

    out[which(preproseq==j),] <- round(c(apply(temp, 2, mean), apply(temp, 2, sd)),2)

    # clustering tendency
    cltend[which(preproseq==j)] <- gethopkins(dat)

    # outlier tendency
    orhquantile[which(preproseq==j)] <- getorh(dat)
  }

  result <- data.frame(cbind(charactergrid, out, cltend, orhquantile))

}


getinteractiveprediction <- function(intrain, intest, predictor){
  fitControl <- caret::trainControl(method = "boot", repeats=2)
  model <- caret::train(y ~., data=intrain, method=predictor, trControl = fitControl)
  prediction <- as.data.frame(predict(model, newdata=intest))
  output <- mean(as.character(prediction[,1])==as.character(intest$y))
}



subclassprediction <- function(object, predictor, nholdout){

  data <- object@data
  data <- data.frame(data@x, y=data@y)
  con <- numeric(nholdout)

  for (i in 1:nholdout){

  training <- caret::createDataPartition(data$y, times=1, list=FALSE, p=0.66)[,1]

  intrain <- data[training,]
  intest <- data[-training,]

  con[i] <- getinteractiveprediction(intrain, intest, predictor)

  }

  con <- mean(con)

}
