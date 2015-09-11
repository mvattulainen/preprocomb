## PREDICTION CONTROL

setClass("PredictionControl", representation(predictors="character", data="list", grid="gridClass"))

initializePredictionControl <- function()
{
  pcobject <- new("PredictionControl")
}

a <- initializePredictionControl()

setGeneric("addpredictortopc<-", function(object,value) {standardGeneric("addpredictortopc<-")})

setReplaceMethod(f="addpredictortopc", signature="PredictionControl", definition=function(object,value){
  object@predictors <- c(object@predictors, c(value))
  return (object)
}
)

addpredictortopc(a) <- c('rf', 'rpart', 'svmRadial')

setGeneric("adddatatopc<-", function(object,value) {standardGeneric("adddatatopc<-")})

setReplaceMethod(f="adddatatopc", signature="PredictionControl", definition=function(object,value){
  object@data <- value
  return (object)
}
)

adddatatopc(a) <- formdatacontent

setGeneric("addgridtopc<-", function(object,value) {standardGeneric("addgridtopc<-")})

setReplaceMethod(f="addgridtopc", signature="PredictionControl", definition=function(object,value){
  object@grid <- value
  return (object)
}
)

addgridtopc(a) <- grid


### PREDICTION

library(caret)
library(caretEnsemble)
library(randomForest)
library(rpart)
library(Metrics)
library(kernlab)

preprocomb <- function(predictioncontrol){

  out <- data.frame()
  res <- data.frame()
  temp <- data.frame()

  formdatacontent <- predictioncontrol@data
  grid <- predictioncontrol@grid
  predictors <- predictioncontrol@predictors

  for (j in 1:nrow(grid@grid))
  {
    dat <- formdatacontent[[j]]
    dat_y <- dat@y
    dat_x <- dat@x
    dat1 <- data.frame(y=dat_y, x=dat_x)

    for (i in 1:1){

      training <- sample(1:length(dat_y), 30)
      intrain <- dat1[training,]
      intest <- dat1[-training,]

      model_list <- caretList(y ~., data=intrain, methodList=predictors)
      prediction <- as.data.frame(predict(model_list, newdata=intest))
      prediction$vote <- apply(prediction, 1, Mode)
      con <- as.numeric(lapply(prediction, function(x) ce(as.character(x), as.character(intest$y))))
      temp <- data.frame(rbind(temp, con))
    }
    res <- apply(temp, 2, mean)
    out <- data.frame(rbind(out,res))
    colnames(out) <- c(predictors, "vote")
  }
  temp5 <- data.frame(apply(grid@grid, 2, as.character))
  out <- data.frame(cbind(temp5, out))
}

out <- preprocomb(a)
