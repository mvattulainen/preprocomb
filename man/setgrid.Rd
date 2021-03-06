% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/04GridClass.R
\name{setgrid}
\alias{setgrid}
\title{constructor function for creating preprocessing combinations}
\usage{
setgrid(phases, data)
}
\arguments{
\item{phases}{(character) vector of phases}

\item{data}{(data frame)}
}
\value{
a GridClass object
}
\description{
setgrid takes the preprocessing phases, which contain preprocessors and creates
the combinations of them as a grid. It then computes and stores the transformed
data sets for each combination. setgrid initializes a GridClass object.
}
\details{
If there are missing values, imputation phase must be set as first phase.
Default phase "sampling" can only be used with data, which has binary class labels.
}
\examples{
grid <- setgrid(phases=c("outliers", "irrelfeatures"), data=iris)
}

