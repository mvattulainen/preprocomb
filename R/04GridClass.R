#' @include 03BaseClass.R
NULL

reportexitstatus <- function(datalist){

  variance <- unlist(lapply(datalist, function(x) slot(x, "variance")))
  finite <- unlist(lapply(datalist, function(x) slot(x, "finite")))
  completeobs <- unlist(lapply(datalist, function(x) slot(x, "completeobs")))
  classbalance <- unlist(lapply(datalist, function(x) slot(x, "classbalance")))
  corrbelowdotnine <- unlist(lapply(datalist, function(x) slot(x, "corrbelowdotnine")))
  ntopratiotwoplus <- unlist(lapply(datalist, function(x) slot(x, "ntopratiotwoplus")))
  mindimensions <- unlist(lapply(datalist, function(x) slot(x, "mindimensions")))

  check <- all(c(variance, finite, completeobs, classbalance, corrbelowdotnine, ntopratiotwoplus, mindimensions))==TRUE

  if (check==TRUE) {result <- c("Exit status: OK: Stable computation of misclassification errors expected.")}
  if (check==FALSE) {result <- c("Exit status: Warning: Unstable computation of misclassification errors expected. See: yourgridclassobject@data")}

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
    stop("All list elements in argument 'phases' must point to PhaseClass objects.")}

    dataclassobject <- initializedataclassobject(data)

    gridclassobject <- new("GridClass")

    ## create grid
    # extract preprocessors of each phase into a list
    # create combinations by expanding the list

    templist <- lapply(phases, function(x) eval(as.name(x))@preprotransformations)
    grid <- expand.grid(templist)
    colnames(grid) <- unlist(phases)
    gridclassobject@grid <- grid

    # process each row in the grid
    gridclassobject@data <- formdata(grid, dataclassobject)

    return(gridclassobject)
  }




  ## DATA FORMATION

  formdata <- function(grid, dataobject){

      result <- vector(mode="list", nrow(grid))

      for (i in 1:nrow(grid)) # processing each row
      {

        result[i] <- prepro(as.character(grid[i, 1]), dataobject) # computation of first columns on a row in the grid

        for (j in 2:ncol(grid))
        {
        result[i] <- prepro(as.character(grid[i,j]), result[[i]])@data # computation of subsequent columns on a row in the grid
        }

      }

    # Output: list of DataClass objects

    result <- lapply(result, validatedataclassobject) # validate the output of each row in the grid
    print(reportexitstatus(result))
    return(result)

  }


