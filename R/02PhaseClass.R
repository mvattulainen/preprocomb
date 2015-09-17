#' @include 01DataClass.R
NULL

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
#' @param phasename (character)
#' @param preprocessor (list) subclass objects
#' @param preimpute (logical) whether phase is executed before imputation

initializephaseclassobject <- function(phasename, preprocessor, preimpute){

  if (class(phasename)!="character") {stop("Argument phasename must must be of class character.")}
  if (class(preprocessor)!="list") {stop("Argument preprocessor must be a list.")}
  if (length(preprocessor)==0) {stop("Argument preprocessor must have one or more list elements.")}
  if (class(preimpute)!="logical") {stop("Argument preimpute must be a logical (TRUE/FALSE).")}

  phaseclassobject <- new("PhaseClass", objectname=phasename, preimpute=preimpute)
  phaseclassobject@preprotransformations <- preprocessor
  return(phaseclassobject)
}




