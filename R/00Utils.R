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
