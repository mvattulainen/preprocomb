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

combpredict <- function(predictioncontrol, nholdout){

  out <- data.frame()
  res <- data.frame()
  temp <- data.frame()

  formdatacontent1 <- predictioncontrol@grid
  formdatacontent <- formdatacontent1@data
  grid <- predictioncontrol@grid
  predictors <- predictioncontrol@predictors

  for (j in 1:nrow(grid@grid))
  {
    dat <- formdatacontent[[j]]
    dat1 <- data.frame(y=formdatacontent[[j]]@y, x=formdatacontent[[j]]@x)

    for (i in 1:nholdout){

      training <- sample(1:nrow(dat1), round(nrow(dat1)*0.66,0)) #TODO: caret data partition
      intrain <- dat1[training,]
      intest <- dat1[-training,]

      model_list <- caretEnsemble::caretList(y ~., data=intrain, methodList=predictors)
      prediction <- as.data.frame(predict(model_list, newdata=intest))
      prediction$vote <- apply(prediction, 1, Mode)
      con <- as.numeric(lapply(prediction, function(x) Metrics::ce(as.character(x), as.character(intest$y))))
      temp <- data.frame(rbind(temp, con))
    }
    res <- apply(temp, 2, mean)
    out <- data.frame(rbind(out,res))
    colnames(out) <- c(predictors, "vote")
  }
  temp5 <- data.frame(apply(grid@grid, 2, as.character))
  out <- data.frame(cbind(temp5, out))
}

