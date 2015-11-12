#' @include 06PreProCombClass.R
NULL


#' showrules
#'
#' showrules shows association rules for classification accuracy.
#' Classification accuracy is categorical as 'high' for best twenty
#' percent and 'low' for rest.

#' @param PreProCombClassobject (PreProCombClass)
#' @param support (numeric) support for association rules, default to 0.05
#' @param confidence (numeric) confidence for association rules, defaults to 0.5
#' @export

showrules <- function(PreProCombClassobject, support=0.05, confidence=0.5){
  rules <- arules::apriori(PreProCombClassobject@catclassification, parameter = list(support = support, confidence = confidence), appearance=list(rhs='target=high', default='lhs'))
  arules::inspect(rules)
}




