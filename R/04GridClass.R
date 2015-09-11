## GRID

setClass("gridClass", representation(grid="data.frame", phases="list"))

grid <- new("gridClass")

setGeneric("addtogrid<-", function(object,value) {standardGeneric("addtogrid<-")})

setReplaceMethod(f="addtogrid", signature="gridClass", definition=function(object,value){
  object@phases <- c(object@phases, c(getname(value)))
  return (object)
}
)

addtogrid(grid) <- imputation
addtogrid(grid) <- scaling

setGeneric("addtoslot<-", function(object,value) {standardGeneric("addtoslot<-")})

setReplaceMethod(f="addtoslot", signature="gridClass", definition=function(object,value){
  object@grid <- value
  return (object)
}
)

setGeneric("creategrid", function(object) {standardGeneric("creategrid")})

setMethod("creategrid", signature(object = "gridClass"), function(object)
{
  a <- object@phases #list of phases
  b <- lapply(a, function(x) eval(as.name(x))@preprotransformations) # list having two lists
  e <- lapply(b, function(x) lapply(x, function(x) x@objectname))
  v <- expand.grid(e)
  colnames(v) <- unlist(a)
  return(v)
})

addtoslot(grid) <- creategrid(grid)

## TRANSFORM

initialize <- function(classname, dataobject){
  a <- new(classname)

  if (class(dataobject)=="DataClass") {b <- transformdata(a, dataobject)}
  else {b <- transformdata(a, dataobject@data)}
  a@data <- b
  a
}

a  <- initialize("naomit", data)
b <- initialize("scale", a)

## DATA FORMATION

formdata <- function(grid){

  temp <- grid@grid
  temp1 <- dim(temp)
  res <- vector(mode="list", length=temp1[1])
  for (i in 1:temp1[1]) # all rows
  {
    a <- initialize(as.character(temp[i, 1]), data) # imputation
    for (j in 2:temp1[2])
    {
      a <- initialize(as.character(temp[i,j]), a)
      res[i] <- a@data

    }
  }
  return(res)

}

formdatacontent <- formdata(grid)
