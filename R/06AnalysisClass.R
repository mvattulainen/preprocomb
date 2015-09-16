
#' AnalysisClass
#'
#' AnalysisClass represents the analysis of preprocessing combinations
#' @slot best (data frame) best (that is, lowest misclassification error) combination
#' @slot all (data frame) all preprocessing combinations and respective misclassification errors
#' @slot importance (data frame) variable importance of phases in predicting misclassification error
#' @slot rules assosiation rules for "low" (that is, lowest 10%) misclassification error

setClass("AnalysisClass", representation(best="data.frame", all="data.frame", importance="data.frame", rules="data.frame"))

#' analyse
#'
#' analyze outputs the analysis of preprocessing combinations
#
#' @param out (data frame) out of function preprocomb() or preproadeq()
#' @export

analyze <- function(out)
{
  analysisclassobject <- new("AnalysisClass")

  tempfactor <- sum(sapply(out, is.factor)) # number of phases
  tempseq <- c(seq(1,tempfactor, 1), ncol(out))

  # best combination
  analysisclassobject@best <- out[which.min(out[,ncol(out)]), tempseq] # BUG

  # all combinations
  analysisclassobject@all <- out

  # variable importance

  tempout <- out[,tempseq]
  cutpoint <- quantile(tempout[,ncol(tempout)], .10)
  tempout$target <- cut(tempout[,ncol(tempout)], breaks=c(0, cutpoint, Inf), labels=c("low", "high"))
  tempout <- tempout[, -length(tempseq)]
  out.rf <- randomForest(target ~ ., data=tempout, ntree=1000, keep.forest=FALSE, importance=TRUE)
  temp1 <- data.frame(importance(out.rf)[,3])
  temp1$phase <- names(importance(out.rf)[,3])
  rownames(temp1) <- NULL
  analysisclassobject@importance <- temp1

  ##
  temp2 <- data.frame(lapply(tempout, factor))
  trans3 <- as(temp2, "transactions")
  rules <- apriori(trans3, parameter = list(support = 0.01, confidence = 0.8), appearance=list(rhs='target=low', default='lhs'))
  rout <-  as(rules, "data.frame")
  analysisclassobject@rules <- rout

  return(analysisclassobject)
}


