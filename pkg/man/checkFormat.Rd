\name{checkFormat}
\alias{checkFormat}
\title{Checks the format of one or more variables in a dataset.}
\usage{
  checkFormat(data, var, pattern, n = 100)
}
\arguments{
  \item{data}{data source (data.frame)}

  \item{var}{name(s) of the variable(s) of \code{data}}

  \item{pattern}{character string containing a
  \link{regular expression}}

  \item{n}{maximum number of invalid values that are
  displayed}
}
\value{
  the results are silently returned in a list
}
\description{
  Checks the format of one or more variables in a dataset.
  For each variable, a summary is given how of many values
  satisfy the specified pattern.
}

