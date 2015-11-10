#' @include 04GridClass.R
NULL

# Get misclassification error/ programmatic mode/ many models

getprogrammaticprediction <- function(preprocesseddataset, predictors, fitControl){

  tryCatch({

  training <- caret::createDataPartition(preprocesseddataset$y, times=1, list=FALSE, p=0.66)[,1]

  intrain <- preprocesseddataset[training,]
  intest <- preprocesseddataset[-training,]

  model_list <- caretEnsemble::caretList(y ~., data=intrain, methodList=predictors, trControl=fitControl)
  prediction <- as.data.frame(predict(model_list, newdata=intest))
  prediction$vote <- apply(prediction, 1, Mode)
  output <- as.numeric(lapply(prediction, function(x) mean(as.character(x)==as.character(intest$y))))

  }, error= function(e) return(NA) )

}

gethopkins <- function(dat){
output <- unlist(clustertend::hopkins(dat@x, n=as.integer(nrow(dat@x)/3)))
}

getorh <- function(dat){
  orh_score <- suppressMessages(DMwR::outliers.ranking(dat@x))
  orh_rank <- orh_score$prob.outliers[orh_score$rank.outliers]
  output <- e1071::skewness(orh_rank)
}

gridrowsinsearch <- function(searchmethod, grid){

  # Optimization method
  if (searchmethod=="exhaustive") {preproseq <- seq(1, nrow(grid@grid), 1)}
  if (searchmethod=="random") {preproseq <- sample(1:nrow(grid@grid), as.integer(nrow(grid@grid)/5))}
  if (searchmethod=="grid") {
    preproseq <- as.list(seq(1, nrow(grid@grid), by=as.integer(nrow(grid@grid)/(nrow(grid@grid)/10))))
    preproseq <- unlist(lapply(preproseq, function(x) x+sample(0:2, 1)))
  }
  return(preproseq)
}

## PREDICTION ================================================

combinationevaluation <- function(predictors, gridclassobject, nholdout, searchmethod, predict, cluster, outlier){

  # initializations
  grid <- gridclassobject
  fitControl <- caret::trainControl(method="boot", repeats=2, savePredictions=TRUE)

  # grid rows to be included based on search argument
  gridrowsincludedinsearch <- gridrowsinsearch(searchmethod, grid)

  # initializations
  if (ncol(grid@grid) > 1){charactergrid <- apply(grid@grid[gridrowsincludedinsearch,], 2, as.character)}
  if (ncol(grid@grid) == 1){
    charactergrid <- apply(data.frame(unlist(grid@grid[gridrowsincludedinsearch,])), 2, as.character)
    colnames(charactergrid) <- "Preprocessor"
    }


  ncomputations <- length(predictors)+1
  outmean <- data.frame(matrix(nrow=length(gridrowsincludedinsearch), ncol=ncomputations))
  outsd <- data.frame(matrix(nrow=length(gridrowsincludedinsearch), ncol=ncomputations))
  cltend <- numeric(length(gridrowsincludedinsearch))
  orhquantile <- numeric(length(gridrowsincludedinsearch))
  result <- list(5)

  # for each selected row in the grid

  print(paste("Number of combinations in evaluation:", length(gridrowsincludedinsearch)))
  cat("Combination number in process:")

  for (j in gridrowsincludedinsearch)
  {
    cat(" ",j,",", sep="")

    dat <- grid@data[[j]]
    dat1 <- data.frame(y=dat@y, x=dat@x)

    holdoutclassificationaccuracies <- data.frame(matrix(nrow=nholdout, ncol=ncomputations))

    # predictions

    if (predict==TRUE) {

    for (holdoutround in 1:nholdout){
      holdoutclassificationaccuracies[holdoutround,] <- getprogrammaticprediction(dat1, predictors, fitControl)
    }
    outmean[which(gridrowsincludedinsearch==j),] <- apply(holdoutclassificationaccuracies, 2, function(x) mean(x, na.rm=TRUE))
    outsd[which(gridrowsincludedinsearch==j),] <- apply(holdoutclassificationaccuracies, 2, function(x) sd(x, na.rm=TRUE))
    }

    # clustering tendency

    if (cluster==TRUE) {
    cltend[which(gridrowsincludedinsearch==j)] <- gethopkins(dat)
    }

    # outlier tendency

    if (outlier==TRUE) {
    orhquantile[which(gridrowsincludedinsearch==j)] <- getorh(dat)
    }

  }

  result[[1]] <- outmean
  result[[2]] <- outsd
  result[[3]] <- cltend
  result[[4]] <- orhquantile
  result[[5]] <- charactergrid

  return(result)
}



