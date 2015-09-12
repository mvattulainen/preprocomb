## PHASE

#' PhaseClass
#'
#' PhaseClass represents the phases of preprocessing such as missing value imputation, outlier removal etc.
#' @slot objectname (character) name of the phase
#' @slot preprotransformations (list) preprocessing transformations belonging to the phase
#' @slot preimpute (logical) TRUE, if phase is execution before missing value imputation

setClass("PhaseClass", representation(objectname="character", preprotransformations="list", preimpute="logical"))

#' initializephaseclassobject
#'
#' initializephaseclassobject is a constructor function for initializing a PhaseClass object.
#' The function separates class labels from rest of the variables.
#'
#' @params phasename (character)
#' @params preprocessor (list) subclass objects
#' @params preimpute (logical) whether phase is executed before imputation



initializephaseclassobject <- function(phasename, preprocessor, preimpute){
  phaseclassobject <- new("PhaseClass", objectname=phasename, preimpute=preimpute)
  phaseclassobject@preprotransformations <- preprocessor
  return(phaseclassobject)
}

setGeneric("getname", function(object) {standardGeneric("getname")})

setMethod("getname", signature(object = "PhaseClass"), function(object)
{
  object@objectname
})


