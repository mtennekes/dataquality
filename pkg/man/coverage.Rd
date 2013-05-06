\name{coverage}
\alias{coverage}
\alias{overcoverage}
\alias{undercoverage}
\title{Determines the coverage of a data source with respect to a reference dataset}
\usage{
  coverage(source.data, reference.data, source.keys = keys,
    reference.keys = source.keys,
    keys = intersect(names(source.data), names(reference.data)),
    percentage = FALSE, ...)

  overcoverage(source.data, reference.data,
    source.keys = keys, reference.keys = source.keys,
    keys = intersect(names(source.data), names(reference.data)),
    percentage = FALSE, ...)

  undercoverage(source.data, reference.data,
    source.keys = keys, reference.keys = source.keys,
    keys = intersect(names(source.data), names(reference.data)),
    percentage = FALSE, ...)
}
\arguments{
  \item{source.data}{source data (data.frame)}

  \item{reference.data}{reference data (data.frame)}

  \item{source.keys}{key variable(s) of \code{source.data}.
  Either a character vector of variable names, a numeric
  vector of column indices, or a logical vector indicating
  which columns to select.}

  \item{reference.keys}{key variable(s) of
  \code{reference.data}. Either a character vector of
  variable names, a numeric vector of column indices, or a
  logical vector indicating which columns to select.}

  \item{keys}{common key variable(s) of \code{source.data}
  and \code{reference.data}.}

  \item{percentage}{logical that determines whether to
  return the coverge result in percentages}

  \item{...}{arguments passed on to other functions
  (unused)}
}
\value{
  coverage/overcoverage/undercoverage fraction (or
  percentage)
}
\description{
  Determines the coverage of a data source with respect to
  a reference dataset. The function coverage determines the
  number, fraction, and percentage of matching records. The
  functions overcoverage and undercoverage respectively
  determines the oves- respectively undercoverage in terms
  of numbers, fractions and percentages of non-matching
  records.
}

