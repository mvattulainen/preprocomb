#' @include 01DataClass.R
NULL

## PHASE

setClass("PhaseClass", representation(objectname="character", preprotransformations="list", preimpute="logical"))

#' constructor function for defining a preprocessing phase.
#'
#' Preprocessing phases consist of preprocessing techniques defined with setpreprocessor().
#' Phases can be defined with setphase() and combined to a grid of combinations with setgrid().
#'
#' @param phasename (character) name of the phase
#' @param preprocessor (character) vector of preprocessors belonging to the phase
#' @param preimpute (logical) whether phase is missing value imputation
#' @return a PhaseClass object
#' @examples
#' imputation <- setphase("imputation", c("naomit", "meanimpute"), TRUE)
#' @export
#' @details All elements of argument 'preprocessor' must point to PreprocessorClass objects constructed with function setpreprocessor(). \cr
#' If dataset contains missing values, missing value imputation must be the first phase when combining phases to grid with setgrid().

setphase <- function(phasename, preprocessor, preimpute){

  if (class(phasename)!="character") {stop("Argument 'phasename' must be a character string.")}
  if (class(preprocessor)!="character") {stop("Argument 'preprocessor' must be a character vector.")}
  if (length(preprocessor)==0) {stop("Argument 'preprocessor' must have one or more elements.")}
  if (class(preimpute)!="logical") {stop("Argument 'preimpute' must be a logical (TRUE/FALSE).")}

  listofpreprocessors <- as.list(preprocessor)
  validatepreprocessorcall(listofpreprocessors)

  phaseclassobject <- new("PhaseClass", objectname=phasename, preimpute=preimpute)
  phaseclassobject@preprotransformations <- listofpreprocessors
  return(phaseclassobject)
}

validatepreprocessorcall <- function(listofpreprocessors) {
  if (any(unlist(lapply(listofpreprocessors, function(x) extends(x, "PreprocessorClass")))=="FALSE")) {
    stop("Element or elements of argument 'preprocessor' must point to PreprocessorClass objects constructed with function setpreprocessor().") }
}

