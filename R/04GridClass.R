#' @include 03BaseClass.R
NULL


reportexitstatus <- function(datalist){

  variance <- unlist(lapply(datalist, function(x) slot(x, "variance")))
  finite <- unlist(lapply(datalist, function(x) slot(x, "finite")))
  completeobs <- unlist(lapply(datalist, function(x) slot(x, "completeobs")))
  classbalance <- unlist(lapply(datalist, function(x) slot(x, "classbalance")))

  temp <- all(variance)==TRUE & all(finite)==TRUE & all(completeobs)==TRUE & all(classbalance)==TRUE

  if (temp==TRUE) {result <- c("Exit status: OK: Stable computation of misclassification errors expected.")}
  if (temp==FALSE) {result <- c("Exit status: Warning: Unstable computation of misclassification errors expected. See: yourgridclassobject@data")}

  return(result)

  }


## GRID

setClass("GridClass", representation(grid="data.frame", data="list"))

#' setgrid
#'
#' setgrid creates the combinations as a grid and computes the transformed data for each combination (row of grid)
#
#' @param phases (character) vector of phases
#' @param data (data frame)
#' @examples
#' ## grid <- setgrid(phases=c("outlier", "selection"), data=iris)
#' @export

    setgrid <- function(phases, data){

    if(class(phases)!="character"){stop("Argument 'phases' must of a character vector.")}
    if(class(data)!="data.frame"){stop("Argument 'data' must of a data frame.")}

    phases <- as.list(phases)

    if(!all(lapply(phases, function(x) class(eval(as.name(x))))=="PhaseClass")){
      stop("All list elements in argument phases must point to PhaseClass objects.")}


    dataclassobject <- initializedataclassobject(data)

    gridclassobject <- new("GridClass")

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



  ## DATA FORMATION  ## FIX THIS

  formdata <- function(grid, data){

      result <- vector(mode="list", nrow(grid))

      for (i in 1:nrow(grid)) # processing by row
      {

        result[i] <- initializesubclassobject(as.character(grid[i, 1]), data) # computation of first result for a row

        for (j in 2:ncol(grid))
        {
        newsubclassobject <- initializesubclassobject(as.character(grid[i,j]), result[[i]])
        result[i] <- newsubclassobject@data # updating the latest result on a same on a row until last column of grid is updated
        }

      }

    # Output: list of DataClass objects
    result <- lapply(result, validatedataclassobject)
    print(reportexitstatus(result))
    return(result)

  }


