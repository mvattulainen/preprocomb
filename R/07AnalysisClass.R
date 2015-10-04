#' @include 06PreProCombClass.R
NULL

#' preprorules
#'
#' preprorules shows the association rules for best ten percent misclassification errors
#' preprorules uses arules package
#
#' @param preprocombobject (PreProCombClass)
#' @param support (numeric) defaults to 0.05
#' @param confidence (numeric) defaults to 0.7

preprorules <- function(preprocombobject, support=0.05, confidence=0.7){
preprotransactions <- as(preprocombobject@rawcat, "transactions")
rules <- apriori(preprotransactions, parameter = list(support = support, confidence = confidence), appearance=list(rhs='target=low', default='lhs'))
output <-  as(rules, "data.frame")
}

#' preprotree
#'
#' preprotree shows c5.0 tree for best ten percent misclassification errors
#' preprorules uses C50 package
#
#' @param preprocombobject (PreProCombClass)

preprotree <- function(preprocombobject){
  mod1 <- C50::C5.0(preprocombobject@rawcat$target ~ ., data = preprocombobject@rawcat)
  plot(mod1)
}

