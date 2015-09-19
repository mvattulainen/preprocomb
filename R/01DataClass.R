#' @include 00Utils.R
NULL

## DATA

#' DataClass
#'
#' DataClass the data
#' @slot x (data frame)
#' @slot y (factor) class labels

setClass("DataClass", representation(x="data.frame", y="factor", variance="logical", finite="logical", noNA="logical", classbalance="logical"))

#' initializedataobject
#'
#' initializedataobject is a constructor function for initializing a DataClass object.
#' The function separates class labels from rest of the (numerical) variables.
#'
#' @param data (data frame)
#' @return (DataClass) object

initializedataobject <- function(data){

  if(class(data)!="data.frame"){stop("Argument 'data' is not of class data frame.")}
  if(sum(sapply(data, is.factor)==TRUE)!=1) {stop("Argument to initializedataobject must have one and only one factor column.")}
  if(sum(sapply(data, is.numeric)==TRUE)!=ncol(data)-1) {stop("Argument initializedataobject must have only numeric columns and one factor column.")}

  dataclassobject <- new("DataClass")
  dataclassobject@x <- data[sapply(data, is.numeric)]
  dataclassobject@y <- factor(data[sapply(data, is.factor)][,1])
  return(dataclassobject)
}


