## PHASE

#' PhaseClass
#'
#' PhaseClass represents the phases of preprocessing such as missing value imputation, outlier removal etc.
#' @slot objectname (character) name of the phase
#' @slot preprotransformations (list) preprocessing transformations belonging to the phase
#' @slot preimpute (logical) TRUE, if phase is execution before missing value imputation

setClass("PhaseClass", representation(objectname="character", preprotransformations="list", preimpute="logical"))

phaseobject <- new("PhaseClass")

imputation <- new("PhaseClass", objectname="imputation", preimpute=TRUE)
scaling <- new("PhaseClass", objectname="scaling", preimpute=FALSE)

setGeneric("addtophase<-", function(object,value) {standardGeneric("addtophase<-")})

setReplaceMethod(f="addtophase", signature="PhaseClass", definition=function(object,value){
  object@preprotransformations <- c(object@preprotransformations, c(value))
  return (object)
}
)

setGeneric("getname", function(object) {standardGeneric("getname")})

setMethod("getname", signature(object = "PhaseClass"), function(object)
{
  object@objectname
})

setMethod("getname", signature(object = "BaseClass"), function(object)
{
  object@objectname
})
