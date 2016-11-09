#' @include 04GridClass.R
NULL

#' parallel computation of classification accuracy holdout rounds
#'
#' This function is used internally and exported for package 'metaheur'.
#'
#' @param preprocesseddataset (DataClass)
#' @param predictors caret models
#' @param nholdout number of holdout rounds
#' @details If model tuning fails, NA is returned as classification accuracy of a combination.
#' If model fitting and prediction for holdout round fails, NA is returned for the holdout round.
#' @export
#' @keywords internal

getprogrammaticprediction <- function(preprocesseddataset, predictors, nholdout){

  tryCatch({

    # TUNE HYPERPARAMETERS FOR EACH MODEL

    fitControl <- caret::trainControl(method="boot", number=2, savePredictions=TRUE)
    klist <- list()
    packagevector <- character()
    for (k in 1:length(predictors))
    {
      mod <- suppressWarnings(caret::train(y ~., data=preprocesseddataset, method=predictors[k], trControl=fitControl))
      klist <- c(klist, list(mod$bestTune))
      packagevector <- c(packagevector, mod$modelInfo$library[1])
    }

    if (length(predictors)!=length(klist)) stop("One of the selected models does not have tuning parameters.")


    ## COMPUTE CLASSIFICATION ACCURACIES BY USING THE TUNED HYPERPARAMETERS

    modelsresults <- data.frame(matrix(ncol=length(predictors)+1, nrow=nholdout))

    for (l in 1:length(predictors)) ## for each classifier
    {

      holdoutaccuracy <- foreach::foreach(j=1:nholdout, .combine='c', .packages=c('caret', packagevector)) %dopar% {

        training <- caret::createDataPartition(preprocesseddataset$y, times=1, list=FALSE, p=0.66)[,1]
        intrain <- preprocesseddataset[training,]
        rownames(intrain) <- make.names(rownames(intrain), unique = TRUE)
        intest <- preprocesseddataset[-training,]
        rownames(intest) <- make.names(rownames(intest), unique = TRUE)

        tryCatch({
        mod <- caret::train(y ~., data=intrain, method=predictors[l], tuneGrid=klist[[l]], trControl=trainControl(method="none"))
        prediction <- predict(mod, newdata=intest)

        holdoutaccuracy <- mean(prediction==intest$y)

        }, error= function(e) return({holdoutaccuracy <- NA}) )

        }
      modelsresults[,l] <- holdoutaccuracy
    }

    # add one column for mean of predictors
    modelsresults[,ncol(modelsresults)] <- apply(modelsresults, 1, function(x) mean(x, na.rm=TRUE))

  return(modelsresults)

}, error= function(e) return({modelsresults <- rep(NA, ncol(modelsresults))} ))

}

# COMPUTE CLUSTERING TENDENCY

gethopkins <- function(dat){
output <- unlist(clustertend::hopkins(dat@x, n=as.integer(nrow(dat@x)/3)))
}

## BY SEARCH TYPE, SET WHICH ROWS IN THE GRID WILL BE EVALUATED

gridrowsinsearch <- function(searchmethod, grid){

  # Optimization method
  if (searchmethod=="exhaustive") {
    preproseq <- seq(1, nrow(grid@grid), 1)}
  if (searchmethod=="random") {
    preproseq <- sample(1:nrow(grid@grid), as.integer(0.2*(nrow(grid@grid))))
    preproseq <- preproseq[order(preproseq)]}
  if (searchmethod=="grid") {
    teninterval <- floor(nrow(grid@grid)/10)
    preproseq <- seq(1, nrow(grid@grid), teninterval)}

  return(preproseq)
}



## PREDICTION ================================================

combinationevaluation <- function(predictors, gridclassobject, nholdout, searchmethod, predict, cluster){

  # PREPARATIONS

  result <- vector("list", length=5)

  # grid rows to be included based on search argument
  gridrowsincludedinsearch <- gridrowsinsearch(searchmethod, gridclassobject)

  # initialize result variables with NA's
  outmean <- data.frame(matrix(nrow=length(gridrowsincludedinsearch), ncol=length(predictors)+1))
  outsd <- data.frame(matrix(nrow=length(gridrowsincludedinsearch), ncol=length(predictors)+1))
  cltend <- rep(NA, length(gridrowsincludedinsearch))

  # FOR EACH ROW IN THE GRID SELECTED BY SEARCH TYPE IN gridrowsincludedinsearch

  cat("Combination number in process:")

  for (j in gridrowsincludedinsearch)
  {
    cat(" ",j,"/",length(gridrowsincludedinsearch), sep="")

    # extract a data frame from gridclassobject

    dat <- gridclassobject@data[[j]]
    dat1 <- data.frame(y=dat@y, x=dat@x)

    # compute classification accuracy

    if (predict==TRUE) {

    holdoutaccuracies <- getprogrammaticprediction(dat1, predictors, nholdout)

      # compute mean and sd from raw holdoutaccuracy data
      outmean[which(gridrowsincludedinsearch==j),] <- apply(holdoutaccuracies, 2, function(x) mean(x, na.rm=TRUE))
      outsd[which(gridrowsincludedinsearch==j),] <- apply(holdoutaccuracies, 2, function(x) sd(x, na.rm=TRUE))

    }

    # clustering tendency

    if (cluster==TRUE) {
    cltend[which(gridrowsincludedinsearch==j)] <- gethopkins(dat)

    }

  }

  # initialize grid slot

  result[[1]] <- outmean
  result[[2]] <- outsd
  result[[3]] <- cltend
  result[[4]] <- NA
  result[[5]] <- getgridascharacter(gridclassobject, gridrowsincludedinsearch)

  return(result)
}

getgridascharacter <- function(grid, gridrowsincludedinsearch){
  if (ncol(grid@grid) > 1){charactergrid <- apply(grid@grid[gridrowsincludedinsearch,], 2, as.character)}
  if (ncol(grid@grid) == 1){
    charactergrid <- apply(data.frame(unlist(grid@grid[gridrowsincludedinsearch,])), 2, as.character)
    colnames(charactergrid) <- "Preprocessor"}
  return(charactergrid)
}


