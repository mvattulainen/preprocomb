## SAMPLE DATA

a <- sample(1:150, 30)
data <- iris
data[a, 1] <- NA

## UTILS

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]}

range01 <- function(y){
  newrange <- (y-min(y))/(max(y)-min(y))
}

meanrep <- function(x){
  x[is.na(x)] =mean(x, na.rm=TRUE)
  x
}

lofcut <- function(x){
lof_score <- DMwR::lofactor(x, k=5)
lof_score[is.finite(lof_score)==FALSE] <- 0 #dirty fix
lof_cut <- quantile(lof_score, .95)
lof_obs <- which(lof_score > lof_cut)
x <- x[-lof_obs,]
}

