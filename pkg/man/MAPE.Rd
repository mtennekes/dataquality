\name{MAPE}
\alias{MAPE}
\alias{sMAPE}
\title{(Symmetric) mean absolute percentage error}
\usage{
  MAPE(actual, forecast)

  sMAPE(actual, forecast)
}
\arguments{
  \item{actual}{the vector of actual values}

  \item{forecast}{the vector of forecasted values}
}
\value{
  the (s)MAPE value
}
\description{
  The function MAPE is an implemention of the mean absolute
  percentage error, and sMAPE of the symmetric mean
  absolute percentage error (version (3) in
  \url{http://www.monashforecasting.com/index.php?title=SMAPE})
}
\examples{
x <- rnorm(10, 4)
y <- x + rnorm(10, 0, 0.3)
MAPE(x,y)
sMAPE(x,y)
}

