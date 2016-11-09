#' @include 07AnalysisComponent.R
NULL

# DEFAULT PREPROCESSORS ==========================

## LOG TRANSFORM

logtransformaux <- function(dataobject){
  x_new <- data.frame(apply(dataobject@x, 2, function(x) logtransformaux2(x)))
  dataobject <- initializedataclassobject(data.frame(x_new, dataobject@y))
}

logtransformaux2 <- function(x){
  if (min(x>0)) return(log(x))
  else {
    mostnegative <- min(x)
    x <- x+abs(mostnegative)+0.00000001
    return(log(x))
  }
}

setpreprocessor("logtransform", "logtransformaux(dataobject)", "log transforms with base exp(1)")

reciprocaltransformaux <- function(dataobject){
  x_new <- data.frame(apply(dataobject@x, 1:2, reciprocalaux2))
  dataobject <- initializedataclassobject(data.frame(x_new, dataobject@y))
}

reciprocalaux2 <- function(x){
  if (x==0) return(x)
  if (x!=0) return(1/x)
}

setpreprocessor("reciprocaltransform", "reciprocaltransformaux(dataobject)", "computes 1/x for non-zero values")

# NEAR ZERO VARIANCE

nezevar <- function(dataobject){
  temp <- caret::nearZeroVar(dataobject@x)
  if (length(temp) !=0) {
    x_new <- dataobject@x[,-temp]
    dataobject <- initializedataclassobject(data.frame(x_new, dataobject@y))
  }
  return(dataobject)
}

setpreprocessor("nearzerovar", "nezevar(dataobject)", "removes near zero variance variables")

## IMPUTATION

naomit <- function(dataobject){
  temp <- apply(dataobject@x, 1, function(x) any(is.na(x)))
  if (any(temp==TRUE)){
    x_new <- dataobject@x[-temp==FALSE,]
    y_new <- dataobject@y[-temp==FALSE]
    dataobject <- initializedataclassobject(data.frame(x_new, y_new))
  }
  return(dataobject)
}

setpreprocessor("naomit", "naomit(dataobject)", "removes observations with missing values")

# Mean imputation

meanimpute_aux <- function(x){
  x[is.na(x)] <- mean(x, na.rm=TRUE) #convert the item with NA to median value from the column
  x
}

meanimpute <- function(dataobject){
  temp <- apply(dataobject@x, 1, function(x) any(is.na(x)))
  if (any(temp==TRUE)){
    x_new <- data.frame(apply(dataobject@x, 2, function(x) meanimpute_aux(x)))
    dataobject <- initializedataclassobject(data.frame(x_new, dataobject@y))
  }
  return(dataobject)
}

setpreprocessor("meanimpute", "meanimpute(dataobject)", "imputes missing values with mean in the variable")

# mean class imputation

meanclass <- function(dataobject){
  temp <- apply(dataobject@x, 1, function(x) any(is.na(x)))
  if (any(temp==TRUE)){
    x_new <- zoo::na.aggregate(dataobject@x, dataobject@y, FUN=mean)
    dataobject <- initializedataclassobject(data.frame(x_new, dataobject@y))
  }
  return(dataobject)
}

setpreprocessor("classmeanimpute", "meanclass(dataobject)", "imputes missing values with variable mean of the class")


# random forest imputation

rfimputefunc <- function(dataobject){
  temp <- apply(dataobject@x, 1, function(x) any(is.na(x)))
  if (any(temp==TRUE)){
    res <- randomForest::rfImpute(dataobject@y ~ ., dataobject@x)
    x_new <- res[,2:ncol(res)]
    y_new <- res[,1]
    dataobject <- initializedataclassobject(data.frame(x_new, y_new))
  }
  return(dataobject)
}

setpreprocessor("randomforestimpute", "rfimputefunc(dataobject)", "imputes missing values with random forest impute")

## SCALING

# basic

basicscaling <- function(dataobject){
  dataobject <- initializedataclassobject(data.frame(x=scale(dataobject@x, center=FALSE), dataobject@y))
}

setpreprocessor("basicscale", "basicscaling(dataobject)", "scales the values without centering")

# center

centerscaling <- function(dataobject){
  dataobject <- initializedataclassobject(data.frame(x=scale(dataobject@x, center=TRUE), dataobject@y))
}

setpreprocessor("centerscale", "centerscaling(dataobject)", "centers and scales the values")

# min-max scaling

range01 <- function(y){
  newrange <- (y-min(y))/(max(y)-min(y))
}

minmaxscaling <- function(dataobject){

  x_new <- data.frame(apply(dataobject@x, 2, range01))
  dataobject <- initializedataclassobject(data.frame(x=x_new, dataobject@y))
}

setpreprocessor("minmaxscale", "minmaxscaling(dataobject)", "scales by minmax scaling")

decimalscaling <- function(dataobject){
  m <- apply(dataobject@x,2,max)
  v <- round(log10(m),0)
  c <- 10^v
  x_new <- sweep(dataobject@x, 2, c, `/`)
  dataobject <- initializedataclassobject(data.frame(x=x_new, y=dataobject@y))
}

setpreprocessor("decimalscale", "decimalscaling(dataobject)", "scales by decimal scaling")

# OUTLIER REMOVAL

tukeyoutlieraux <- function(dataobject){

  out <- apply(dataobject@x, 2, tukeyaux)

  if (all(out==FALSE)) {return(dataobject)}

  if (any(out==TRUE)) {

    out2 <- apply(out, 1, any)
    out3 <- out2==FALSE
    x_new <- dataobject@x[out3,]
    y_new <- dataobject@y[out3]
    result <- initializedataclassobject(data.frame(x=x_new, y=y_new))
    return(result)
  }
}
  tukeyaux <- function(x){
    out <- boxplot(x)$out
    id <- x %in% out
  }

  setpreprocessor("tukeyoutlier", "tukeyoutlieraux(dataobject)", "removes all points that have +1.5IQR outliers")

  ## CLASS IMBALANCE CORRECTIONS

  # oversampling

  oversample <- function(dataobject){

    data <- data.frame(dataobject@x, y=dataobject@y)

    if (nlevels(data$y) > 2) {stop("Oversampling can only be applied to binary class.")}

    freq <- table(data$y)
    temp <- order(freq)
    nsample <- freq[temp[2]]-freq[temp[1]]
    indexes <- which(data$y==names(freq)[temp[1]])
    tempsample <- sample(indexes, nsample, replace=TRUE)
    newdata <- initializedataclassobject(data.frame(rbind(data, data[tempsample,])))
    return(newdata)
  }

  setpreprocessor("oversample", "oversample(dataobject)", "oversamples the less frequent class")

  # undersampling

  undersample <- function(dataobject){

    data <- data.frame(dataobject@x, y=dataobject@y)

    if (nlevels(data$y) > 2) {stop("Undersampling can only be applied to binary class.")}

    freq <- table(data$y)
    temp <- order(freq)
    nsample <- freq[temp[1]]
    indexes <- which(data$y==names(freq)[temp[2]])
    indexes2 <- which(data$y==names(freq)[temp[1]])
    tempsample <- sample(indexes, nsample)
    finalsample <- c(indexes2, tempsample)
    newdata <- initializedataclassobject(data[finalsample,])
    return(newdata)
  }

  setpreprocessor("undersample", "undersample(dataobject)", "undersamples the more frequent class")

  ## FEATURE SELECTION

  # random forest importance

  rfimportance <- function(dataobject, qt){
    rf.imp <- randomForest::randomForest(dataobject@y ~ ., data=dataobject@x, ntree=100, keep.forest=FALSE, importance=TRUE)
    temp <- data.frame(randomForest::importance(rf.imp))
    temp1 <- temp$MeanDecreaseAccuracy > quantile(temp$MeanDecreaseAccuracy, qt)
    dataobject@x  <- dataobject@x[,temp1]
    return(dataobject)
  }

  setpreprocessor("rfselect75", "rfimportance(dataobject, .25)", "selects 75% most important variables by random forest")
  setpreprocessor("rfselect50", "rfimportance(dataobject, .50)", "selects 50% most important variables by random forest")

  # smoothing with lowess

  smoothlowess <- function(dataobject){
    y <- dataobject@x
    result <-data.frame(round(apply(y, 2, function(y) lowess(y[order(y)], f=1/2)[[2]][match(y, y[order(y)])]),2))
    dataobject <- initializedataclassobject(data.frame(result, y=dataobject@y))
  }

  setpreprocessor("lowesssmooth", "smoothlowess(dataobject)", "smooths by LOWESS algorithm")

  smoothcoarse <- function(dataobject){
    x_new <- data.frame(apply(dataobject@x, 2, function(y) round(y, digits=-log10(abs(y))+1)))
    dataobject <- initializedataclassobject(data.frame(x=x_new, y=dataobject@y))
  }

  setpreprocessor("coarsesmooth", "smoothcoarse(dataobject)", "smooths by retaining only two non-zero digits")

  integerbin <- function(dataobject){
    x_new <- data.frame(apply(dataobject@x, 2, function(z) cut(z, breaks=10, labels=FALSE)))
    dataobject <- initializedataclassobject(data.frame(x=x_new, y=dataobject@y))
  }

  setpreprocessor("binningsmooth", "integerbin(dataobject)", "smooths by binning")

  setpreprocessor("noaction", "identity(dataobject)", "does not do any preprocessing")

  # DEFAULT PHASES ==========================


  #### PHASES

  missingvalues <- setphase("missingvalues", c("naomit", "meanimpute", "classmeanimpute", "randomforestimpute"), TRUE)
  missingvariance <- setphase("missingvariance", c("noaction", "nearzerovar"), FALSE)
  noise <- setphase("noise", c("noaction", "coarsesmooth", "binningsmooth", "lowesssmooth"), FALSE)
  valuerange <- setphase("valuerange", c("noaction", "basicscale", "centerscale", "minmaxscale", "decimalscale", "logtransform", "reciprocaltransform"), FALSE)
  outliers <- setphase("outliers", c("noaction", "tukeyoutlier"), FALSE)
  imbalance <- setphase("imbalance", c("noaction", "oversample", "undersample"), FALSE)
  irrelfeatures <- setphase("irrelfeatures", c("noaction", "rfselect50", "rfselect75"), FALSE)

  #' six default phases with preprocessing techniques
  #'
  #' Totals 1080 combinations. preprodefault object can be used as default phases for setgrid().
  #' @examples
  #' ## grid <- setgrid(preprodefault, iris)
  #' @export
  #' @keywords internal
  preprodefault <- c("missingvalues", "missingvariance", "noise", "valuerange", "outliers", "irrelfeatures")

  #' eigth default phases with preprocessing techniques
  #'
  #' Totals 6480 combinations. preprodefaultextended object can be used as default phases for setgrid().
  #' @examples
  #' ## grid <- setgrid(preprodefaultextended, iris[1:90,])
  #' @export
  #' @keywords internal
  preprodefaultextended <- c("missingvalues", "missingvariance", "noise", "valuerange", "outliers", "irrelfeatures", "imbalance")

