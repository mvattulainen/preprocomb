#' @include 01DataClass.R
NULL

## PHASE

setClass("PhaseClass", representation(objectname="character", preprotransformations="list", preimpute="logical"))

#' initializephaseclassobject
#'
#' initializephaseclassobject is a constructor function for initializing a PhaseClass object.
#'
#' @param phasename (character) name of the phase
#' @param preprocessor (character) vector of subclass objects (see ?addpreprocessor)
#' @param preimpute (logical) whether phase is executed before imputation

initializephaseclassobject <- function(phasename, preprocessor, preimpute){

  if (class(phasename)!="character") {stop("Argument 'phasename' must be a character string.")}
  if (class(preprocessor)!="character") {stop("Argument 'preprocessor' must be a character vector.")}
  if (length(preprocessor)==0) {stop("Argument 'preprocessor' must have one or more elements.")}
  if (class(preimpute)!="logical") {stop("Argument 'preimpute' must be a logical (TRUE/FALSE).")}

  listofpreprocessors <- as.list(preprocessor)
  if (any(unlist(lapply(listofpreprocessors, function(x) extends(x, "BaseClass")))=="FALSE")) {
    stop("All elements of argument 'preprocessor' must point to sub classes of BaseClass constructed with function 'addpreprocessor'.") }

  phaseclassobject <- new("PhaseClass", objectname=phasename, preimpute=preimpute)
  phaseclassobject@preprotransformations <- listofpreprocessors
  return(phaseclassobject)
}




