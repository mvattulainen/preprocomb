#' @include 00Utils.R
NULL

setClass("DataClass", representation(x="data.frame", y="factor", variance="logical", finite="logical", completeobs="logical", classbalance="logical", ntopratiotwoplus="logical", mindimensions="logical"))

## VALIDATE THAT MODELS CAN BE SAFELY FITTED FOR A PREPROCESSED DATA SET

validatedata <- function(object){

  data <- object

  # has variance
  temp <- length(caret::nearZeroVar(data@x))
  data@variance <- temp==0

  # is finite
  temp1 <- all(apply(data@x, 1:2, is.finite))
  data@finite <- temp1==TRUE

  # has complete observations
  temp2 <- any(apply(data@x, 1:2, is.na))
  data@completeobs <- temp2==FALSE

  # has class balance
  temp3 <- length(caret::nearZeroVar(data.frame(data@y)))
  data@classbalance <- temp3==0

  # has n to p ratio more than 2
  temp5 <- nrow(data@x) > (2*ncol(data@x))
  data@ntopratiotwoplus <- temp5==TRUE

  # has minimum dimensions
  temp6 <- all(dim(data@x) > c(20,3))
  data@mindimensions <- temp6

  # least frequence class label has more than 4 observations
  minimumycheck <- min(table(data@y))
  if (minimumycheck < 5) {stop("One level in the factor variable has less than five observations.")}

  return(data)

}

#' constructor function for creating a DataClass object
#'
#' initializedataclassobject() is a constructor function for creating a DataClass object. The main
#' use case is adding of new preprocessing techniques to the framework by the user. Added preprocessing
#' techniques (i.e. functions) take as input and must return a DataClass object. See setpreprocessor().
#' @param data (data.frame)
#' @details Argument 'data' must have only numeric columns and one factor column.
#' @export
#' @examples
#' ## dataobject <- initializedataclassobject(iris)

initializedataclassobject <- function(data){

  if(class(data)!="data.frame"){stop("Argument 'data' must be a data frame.")}
  if(sum(sapply(data, is.factor)==TRUE)!=1) {stop("Argument 'data' must have one and only one factor column.")}
  if(sum(sapply(data, is.numeric)==TRUE)!=ncol(data)-1) {stop("Argument 'data' must have only numeric columns and one factor column.")}

  dataclassobject <- new("DataClass")
  dataclassobject@x <- data[sapply(data, is.numeric)]
  dataclassobject@y <- factor(data[sapply(data, is.factor)][,1])
  dataclassobject <- validatedata(dataclassobject)
  return(dataclassobject)
}




