#' @include 03BaseClass.R
NULL

## GRID

#' GridClass
#'
#' GridClass represents the grid
#' @slot grid (data frame) combinations of preprocessors
#' @slot data (list) list of data frames computed from each row in grid

setClass("gridClass", representation(grid="data.frame", data="list"))

  #' initializegridclassobject
  #'
  #' initializegridclassobject is a constructor function for initializing a GridClass object.
  #
  #' @param phases (list)
  #' @param data (DataClass)
  #' @export

  initializegridclassobject <- function(phases, data){

    if(class(phases)!="list"){stop("Argument phases must of a list.")}
    if(!all(lapply(phases, function(x) class(eval(as.name(x))))=="PhaseClass")){
      stop("All list elements in argument phases must point to PhaseClass objects.")}
    if(class(data)!="data.frame"){stop("Argument data must of a data frame.")}

    dataclassobject <- initializedataobject(data)

    gridclassobject <- new("gridClass")
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

  #' initializesubclassobject
  #'
  #' initializesubclassobject is a constructor function for initializing sub class objects from BaseClass.
  #
  #' @param classname (character)
  #' @param dataobject (DataClass or sub class of BaseClass)
  #' @export

  initializesubclassobject <- function(classname, dataobject){

    subclassobject <- new(classname)
    if (class(dataobject)=="DataClass") {transformeddata <- transformdata(subclassobject, dataobject)} # first column in grid with data as argument
    else {transformeddata <- transformdata(subclassobject, dataobject@data)} # subsequent columns in grid with previous subclass object as argument
    subclassobject@data <- transformeddata
    return(subclassobject)
  }

  ## DATA FORMATION

  #' formdata
  #'
  #' formdata function takes grid and a DataClass object as arguments and computes the data for each row in grid
  #
  #' @param grid (GridClass)
  #' @param data (DataClass)

  formdata <- function(grid, data){

    temp <- grid
    temp1 <- dim(temp)
    res <- vector(mode="list", length=temp1[1])
    for (i in 1:temp1[1]) # processing by row
    {

      a <- initializesubclassobject(as.character(temp[i, 1]), data) # imputation phase, first column in grid
      for (j in 2:temp1[2])
      {
        a <- initializesubclassobject(as.character(temp[i,j]), a)
        res[i] <- a@data # placing the data for last column on a row
      }
    }
    return(res)

  }


