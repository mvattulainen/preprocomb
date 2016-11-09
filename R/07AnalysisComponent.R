#' @include 06ResultClass.R
NULL

#' shows association rules for classification accuracy.
#'
#' Classification accuracy label  'high' corresponds to best twenty
#' percent and 'low' for the rest.

#' @param PreProCombClassobject (PreProCombClass)
#' @param support (numeric) support for association rules, default to 0.05
#' @param confidence (numeric) confidence for association rules, defaults to 0.5
#' @export
#' @keywords internal

showrules <- function(ResultClassobjectobject, support=0.05, confidence=0.5){

  if(class(ResultClassobjectobject)!="ResultClass"){stop("The argument 'ResultClassobject' must be a ResultClassobject object.")}
  if(all(ResultClassobjectobject@catclassification==0)){stop("The argument 'ResultClassobjectobject' does not contain classification results.")}

  rules <- arules::apriori(ResultClassobjectobject@catclassification, parameter = list(support = support, confidence = confidence), appearance=list(rhs='target=high', default='lhs'))
  arules::inspect(rules)
}

#' plot preprocessing combination evaluation results
#' @param ResultClassobject (ResultClass) object to be plotted
#' @param type (character) boxplot or lineplot
#' @export

preprocombplot <- function(ResultClassobject, type="boxplot"){

  # get data

  plotdata <- prepareplotdata(ResultClassobject)

  if (type=="boxplot"){
  g <- ggplot(plotdata, aes(y=value, x=Index)) + geom_boxplot() + theme_bw() + ggtitle("Classification accuracy")
  }

  if (type=="lineplot"){
  g <- ggplot(plotdata, aes(y=value, x=Index)) + geom_line() + geom_point() + theme_bw() + ggtitle("Classification accuracy by combination number")
  }

  suppressWarnings(print(g))
  }

prepareplotdata  <- function(ResultClassobject){
  if (class(ResultClassobject)!="ResultClass") {stop("Object to be plotted must be a ResultClassobject object.")}
  if (all(is.na(ResultClassobject@rawall$ALL_MEANMean))==TRUE) {stop("Classification accuracy not computed or failed.")}
  plotdata <- data.frame(y=ResultClassobject@rawall$ALL_MEANMean)
  plotdata$Index <- seq(1, nrow(plotdata),1)
  plotdata <- reshape2::melt(plotdata, id.var="Index")
}


