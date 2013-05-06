\name{dateDiff}
\alias{dateDiff}
\title{Date difference}
\usage{
  dateDiff(date1, date2 = Sys.Date(),
    format = "\%d-\%m-\%Y", unit = "days")
}
\arguments{
  \item{date1}{first date}

  \item{date2}{second date}

  \item{format}{format}

  \item{unit}{unit}
}
\value{
  description of the returned object
}
\description{
  Function that returns the difference between two dates in
  various formats
}
\examples{
dateDiff("15-01-2012", "16-01-2012")
dateDiff("15-01-1998")
dateDiff("15-01-2012", "17-02-2012", unit = "weeks")
dateDiff("15-01-1998", unit = "months")
dateDiff("15-01-1998", unit = "years")
}

