#' @include 04GridClass.R
NULL

## PREDICTION CONTROL

#' PredictionClass
#'
#' PredictionClass stores the computation of preprocomb() or preproadeq()
#' @slot predictors (character) vector of predictors

setClass("PredictionClass", representation(output="data.frame"))

#' PredictionControlClass
#'
#' PredictionControlClass controls as argument the computations
#' @slot predictors (character) vector of predictors
#' @slot grid (GridClass)

setClass("PredictionControl", representation(predictors="character", grid="gridClass"))

#' initializepredictioncontrolclassobject
#'
#' initializepredictioncontrolclassobject is a constructor function for initializing a PredictionControlClass object.
#
#' @param predictors (character) vector of predictors
#' @param grid (GridClass)
#' @examples
#' gridclassobject <- initializegridclassobject(list("outlier", "selection"), iris)
#' predictioncontrol <- initializepredictioncontrolclassobject(predictors='rf', gridclassobject)
#' @export

initializepredictioncontrolclassobject <- function(predictors, grid)
{
  if(class(predictors)!="character"){stop("The argument predictors must a character vector.")}
  if(is.odd(length(predictors))!=TRUE){stop("The number of predictors must be an even number.")}
  if(class(grid)!="gridClass"){stop("The argument grid must be a GridClass object.")}

  predictioncontrolclassobject <- new("PredictionControl")
  predictioncontrolclassobject@predictors <- predictors
  predictioncontrolclassobject@grid <- grid
return(predictioncontrolclassobject)
}

### PREDICTION

#' preprocomb
#'
#' preprocomb is the main execution function for computing the misclassification rate of each preprocessed grid row
#
#' @param predictioncontrol (PredictionControlClass)
#' @examples
#' gridclassobject <- initializegridclassobject(list("outlier", "selection"), iris)
#' predictioncontrol <- initializepredictioncontrolclassobject(predictors='rf', gridclassobject)
#' out <- preprocomb(predictioncontrol)
#' @export

preprocomb <- function(predictioncontrol){

  out <- data.frame()
  res <- data.frame()
  temp <- data.frame()

  formdatacontent1 <- predictioncontrol@grid
  formdatacontent <- formdatacontent1@data
  grid <- predictioncontrol@grid
  predictors <- predictioncontrol@predictors

  for (j in 1:nrow(grid@grid))
  {
    print(grid@grid[j,])
    dat <- formdatacontent[[j]]
    dat_y <- dat@y
    dat_x <- dat@x
    dat1 <- data.frame(y=dat_y, x=dat_x)

    for (i in 1:1){

      training <- sample(1:length(dat_y), 30)
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
  predictionclassobject <- new("PredictionClass", output=out)
  return(predictionclassobject)
}

#' preproadeq
#'
#' preproadeq is the supplementary execution function for computing the adequacy of the (that is, learnability) of each row in grid
#
#' @param predictioncontrol (PredictionControlClass)
#' @export

preproadeq <- function(predictioncontrol){

  formdatacontent1 <- predictioncontrol@grid
  formdatacontent <- formdatacontent1@data
  grid <- predictioncontrol@grid
  res <- numeric(nrow(grid@grid))

  for (j in 1:nrow(grid@grid))
  {
    print(j)
    dat <- formdatacontent[[j]]
    dat_y <- dat@y
    dat_x <- dat@x

    model <- randomForest::randomForest(dat_y ~., dat_x, ntree=30)
    res[j] <- round(mean(model$err.rate[,1]),2)
    }

  temp5 <- data.frame(apply(grid@grid, 2, as.character))
  res <- data.frame(cbind(temp5, res))
  predictionclassobject <- new("PredictionClass", output=res)
  return(predictionclassobject)
}


