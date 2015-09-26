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

## PREDICTION ================================================

combpredict <- function(predictioncontrol, nholdout, search){

  out <- data.frame()
  res <- data.frame()
  temp <- data.frame()
  preproseq <- numeric()

  formdatacontent1 <- predictioncontrol@grid
  formdatacontent <- formdatacontent1@data
  grid <- predictioncontrol@grid
  predictors <- predictioncontrol@predictors

  if (search=="exhaustive") {preproseq <- seq(1, nrow(grid@grid), 1)}
  if (search=="random") {preproseq <- sample(1:nrow(grid@grid), as.integer(nrow(grid@grid)/5))}
  if (search=="grid") {
  preproseq <- as.list(seq(1, nrow(grid@grid), by=as.integer(nrow(grid@grid)/(nrow(grid@grid)/10))))
  preproseq <- unlist(lapply(preproseq, function(x) x+sample(0:2, 1)))
  }

  print(preproseq)

  for (j in preproseq)
  {
    dat <- formdatacontent[[j]]
    dat1 <- data.frame(y=formdatacontent[[j]]@y, x=formdatacontent[[j]]@x)

    for (i in 1:nholdout){

      tryCatch({

      training <- caret::createDataPartition(dat1$y, times=1, list=FALSE, p=0.66)[,1] ## NOTE: <=3

      intrain <- dat1[training,]
      intest <- dat1[-training,]

      model_list <- caretEnsemble::caretList(y ~., data=intrain, methodList=predictors)
      prediction <- as.data.frame(predict(model_list, newdata=intest))
      prediction$vote <- apply(prediction, 1, Mode)
      con <- as.numeric(lapply(prediction, function(x) Metrics::ce(as.character(x), as.character(intest$y))))
      }, error = function(e) return(con <- rep(NA, length(methodList)+1)))
      temp <- data.frame(rbind(temp, con))
    }
    res <- apply(temp, 2, mean)
    out <- data.frame(rbind(out,res))
    colnames(out) <- c(predictors, "vote")
    temp <- data.frame()
  }
  temp5 <- data.frame(apply(grid@grid[preproseq,], 2, as.character))
  out <- data.frame(cbind(temp5, out))
}

subclassprediction <- function(object, predictor){

  predictors <- predictor

  data <- object@data
  data <- data.frame(data@x, y=data@y)
  con <- numeric(3)

  for (i in 1:3){

  training <- caret::createDataPartition(data$y, times=1, list=FALSE, p=0.66)[,1]

  intrain <- data[training,]
  intest <- data[-training,]

  model_list <- caretEnsemble::caretList(y ~., data=intrain, methodList=predictors)
  prediction <- as.data.frame(predict(model_list, newdata=intest))

  con[i] <- as.numeric(lapply(prediction, function(x) Metrics::ce(as.character(x), as.character(intest$y))))
  }

  con <- mean(con)
}
