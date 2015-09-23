#' @include 00Utils.R
NULL

## DATA

setClass("DataClass", representation(x="data.frame", y="factor", variance="logical", finite="logical", completeobs="logical", classbalance="logical", nomulticollinearity="logical", ntopratiotwoplus="logical"))

validatedataclassobject <- function(dataclassobject){

  temp <- length(caret::nearZeroVar(dataclassobject@x))
  dataclassobject@variance <- temp==0

  temp1 <- all(apply(dataclassobject@x, 1:2, is.finite))
  dataclassobject@finite <- temp1==TRUE

  temp2 <- any(apply(dataclassobject@x, 1:2, is.na))
  dataclassobject@completeobs <- temp2==FALSE

  temp3 <- length(caret::nearZeroVar(data.frame(dataclassobject@y)))
  dataclassobject@classbalance <- temp3==0

  temp4 <- length(caret::findCorrelation(cor(dataclassobject@x, use="pairwise.complete.obs"), cutoff = .95))
  dataclassobject@nomulticollinearity <- temp4==0

  temp5 <- nrow(dataclassobject@x) > (2*ncol(dataclassobject@x))
  dataclassobject@ntopratiotwoplus <- temp5==TRUE

  return(dataclassobject)
}



initializedataclassobject <- function(data){

  if(class(data)!="data.frame"){stop("Argument 'data' must be a data frame.")}
  if(sum(sapply(data, is.factor)==TRUE)!=1) {stop("Argument 'data' must have one and only one factor column.")}
  if(sum(sapply(data, is.numeric)==TRUE)!=ncol(data)-1) {stop("Argument 'data' must have only numeric columns and one factor column.")}

  dataclassobject <- new("DataClass")
  dataclassobject@x <- data[sapply(data, is.numeric)]
  dataclassobject@y <- factor(data[sapply(data, is.factor)][,1])
  dataclassobject <- validatedataclassobject(dataclassobject)
  return(dataclassobject)
}


