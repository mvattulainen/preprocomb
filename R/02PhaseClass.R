#' @include 01DataClass.R
NULL

## PHASE

setClass("PhaseClass", representation(objectname="character", preprotransformations="list", preimpute="logical"))

#' setphase
#'
#' setphase is a constructor function for initializing a PhaseClass object.
#'
#' @param phasename (character) name of the phase
#' @param preprocessor (character) vector of preprocessors (see ?setpreprocessor) belonging to the phase
#' @param preimpute (logical) whether phase is executed before missing value imputation
#' @examples
#' ## imputation <- setphase("imputation", c("naomit", "meanimpute", "knnimpute", "randomforestimpute"), TRUE)
#' ## scaling <- setphase("scaling", c("noscale", "scale", "centerscale", "minmaxscale", "softmaxscale"), FALSE)
#' ## outlier <- setphase("outlier", c("nooutlierremove", "lof", "orh"), FALSE)
#' ## sampling <- setphase("sampling", c("nosample", "oversample"), FALSE)
#' ## selection <- setphase("selection", c("noselection", "rfvarused"), FALSE)

setphase <- function(phasename, preprocessor, preimpute){

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




