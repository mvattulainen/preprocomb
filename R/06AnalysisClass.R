setClass("AnalysisClass", representation(best="data.frame", all="data.frame", importance="data.frame", rules="data.frame"))

analyze <- function(out)
{
  analysisclassobject <- new("AnalysisClass")

  # best combination
  analysisclassobject@best <- out[which.min(out$vote), c(1,2,6)] # HARD CODED HERE

  # all combinations
  analysisclassobject@all <- out

  # variable importance
  tempout <- out[,c(1,2,6)]
  tempout$vote <- cut(tempout$vote, 2, labels=c("low", "high"))
  out.rf <- randomForest(vote ~ ., data=tempout, ntree=1000, keep.forest=FALSE, importance=TRUE)
  temp1 <- data.frame(importance(out.rf)[,3])
  temp1$phase <- names(importance(out.rf)[,3])
  rownames(temp1) <- NULL
  analysisclassobject@importance <- temp1

  ##
  library(arules)
  temp2 <- data.frame(lapply(tempout, factor))
  trans3 <- as(temp2, "transactions")
  rules <- apriori(trans3, parameter = list(support = 0.01, confidence = 0.1), appearance=list(rhs='vote=high', default='lhs'))
  rout <-  as(rules, "data.frame")
  analysisclassobject@rules <- rout

  return(analysisclassobject)
}

ana <- analyze(out)
