## DATA

#' DataClass
#'
#' DataClass the data
#' @slot x (data frame)
#' @slot y (factor) class labels

setClass("DataClass", representation(x="data.frame", y="factor"))

#' initializedataobject
#'
#' initializedataobject is a constructor function for initializing a DataClass object.¨¨
#' The function separates class labels from rest of the (numerical) variables.
#'
#' @param data (data frame)
#' @return (DataClass) object

initializedataobject <- function(data){
  ## TODO: data argument validation

  dataclassobject <- new("DataClass")
  dataclassobject@x <- data[sapply(data, is.numeric)]
  dataclassobject@y <- factor(data[sapply(data, is.factor)][,1])
  return(dataclassobject)
}


