#' @include 03BaseClass.R
NULL


reportexitstatus <- function(datalist){

  variance <- unlist(lapply(datalist, function(x) slot(x, "variance")))
  finite <- unlist(lapply(datalist, function(x) slot(x, "finite")))
  noNA <- unlist(lapply(datalist, function(x) slot(x, "noNA")))
  classbalance <- unlist(lapply(datalist, function(x) slot(x, "classbalance")))

  temp <- all(variance)==TRUE & all(finite)==TRUE & all(noNA)==TRUE & all(classbalance)==TRUE

  if (temp==TRUE) {print("Exit status: Stable computation of misclassification errors expected.")}
  if (temp==FALSE) {
                    varianceproblems <- which(variance==FALSE)
                    finiteproblems <- which(finite==FALSE)
                    noNAproblems <- which(noNA==FALSE)
                    classbalanceproblems <- which(classbalance==FALSE)
                    print("Exit status: Unstable computation of misclassification errors likely.")
                    if (length(varianceproblems) > 0) { print(paste("Zero or near zero variance in combinations:", varianceproblems))}
                    if (length(finiteproblems) > 0) { print(paste("Not finite values in combinations:", finiteproblems))}
                    if (length(noNAproblems) > 0) { print(paste("Missing values in combinations:", noNAproblems))}
                    if (length(classbalanceproblems) > 0) { print(paste("Class imbalance in combinations:", classbalanceproblems))}
                    print("1. Get a list of data: yourgridclassobjecthere@data")
                    print("2. Subset the list with the numbers above to identify the problematic data.")
                    }

  }


## GRID

#' GridClass
#'
#' GridClass represents the grid
#' @slot grid (data frame) combinations of preprocessors
#' @slot data (list) list of data frames computed from each row in grid

setClass("GridClass", representation(grid="data.frame", data="list"))

  #' initializegridclassobject
  #'
  #' initializegridclassobject is a constructor function for initializing a GridClass object.
  #
  #' @param phases (character) vector
  #' @param data (data frame)
  #' @examples
  #' gridclassobject <- initializegridclassobject(phases=c("outlier", "selection"), data=iris)
  #' @export

  initializegridclassobject <- function(phases, data){

    if(class(phases)!="character"){stop("Argument phases must of a character vector.")}

    phases <- as.list(phases)

    if(!all(lapply(phases, function(x) class(eval(as.name(x))))=="PhaseClass")){
      stop("All list elements in argument phases must point to PhaseClass objects.")}
    if(class(data)!="data.frame"){stop("Argument data must of a data frame.")}

    dataclassobject <- initializedataclassobject(data)

    gridclassobject <- new("GridClass")
    #gridclassobject@phases <- phases

    ## gridformation
    # create a list with phases as elements and preprocessors as sub elements
    # extract the name of the preprocessor

    templist <- lapply(phases, function(x) eval(as.name(x))@preprotransformations)
    grid <- expand.grid(templist)
    colnames(grid) <- unlist(phases)
    gridclassobject@grid <- grid

    gridclassobject@data <- formdata(grid, dataclassobject)

    return(gridclassobject)
  }




  ## TRANSFORM



  ## DATA FORMATION

  formdata <- function(grid, data){

      result <- vector(mode="list", nrow(grid))

      for (i in 1:nrow(grid)) # processing by row
      {

      result[i] <- initializesubclassobject(as.character(grid[i, 1]), data) # computation of first result for a row

        for (j in 2:ncol(grid))
        {
        newsubclassobject <- initializesubclassobject(as.character(grid[i,j]), result[i])
        result[i] <- newsubclassobject@data # updating the latest result on a same on a row until last column of grid is updated
        }

      }

    # Output: list of DataClass objects
    result <- lapply(result, validatedataclassobject)
    reportexitstatus(result)
    return(result)

  }


