\name{change}
\alias{alive}
\alias{birth}
\alias{change}
\alias{change_fast}
\alias{death1}
\alias{death2}
\title{Functions to determine population dynamics.}
\usage{
  change(source_t1.data, source_t2.data,
    source_t1.keys = keys, source_t2.keys = source_t1.keys,
    keys = intersect(names(source_t1.data), names(source_t2.data)),
    percentage = FALSE, ...)

  change_fast(source_t1.data, source_t2.data,
    source_t1.keys = keys, source_t2.keys = source_t1.keys,
    keys = intersect(names(source_t1.data), names(source_t2.data)),
    percentage = FALSE, ...)

  alive(source_t1.data, source_t2.data,
    source_t1.keys = keys, source_t2.keys = source_t1.keys,
    keys = intersect(names(source_t1.data), names(source_t2.data)),
    percentage = FALSE, ...)

  death1(source_t1.data, source_t2.data,
    source_t1.keys = keys, source_t2.keys = source_t1.keys,
    keys = intersect(names(source_t1.data), names(source_t2.data)),
    percentage = FALSE, ...)

  death2(source_t1.data, source_t2.data,
    source_t1.keys = keys, source_t2.keys = source_t1.keys,
    keys = intersect(names(source_t1.data), names(source_t2.data)),
    percentage = FALSE, ...)

  birth(source_t1.data, source_t2.data,
    source_t1.keys = keys, source_t2.keys = source_t1.keys,
    keys = intersect(names(source_t1.data), names(source_t2.data)),
    percentage = FALSE, ...)
}
\arguments{
  \item{source_t1.data}{data source at time period t1
  (data.frame)}

  \item{source_t2.data}{data source at time period t2
  (data.frame)}

  \item{source_t1.keys}{key variable(s) of
  \code{source_t1.data}. A character vector of variable
  names, a vector of column indices, or a logical vector to
  select columns.}

  \item{source_t2.keys}{key variable(s) of
  \code{source_t2.data}. A character vector of variable
  names, a vector of column indices, or a logical vector to
  select columns.}

  \item{keys}{common key variable(s).  A character vector
  of variable names that are contained in both
  \code{source_t1.data} and \code{source_t2.data}.}

  \item{percentage}{should percentage number be returned?
  (the default value is \code{FALSE}, which means that a
  fraction is returned)}

  \item{...}{arguments passed on other functions (unused)}
}
\value{
  a fraction (or percentage)
}
\description{
  These functions determine the population dynamics between
  two time periods of a data source. \code{change} provides
  the fraction of changed values, and also a summary of new
  and removed records, \code{change_fast} is a fast
  equavalent that only provides the fraction of changed
  values. \code{alive} provides the fraction of records
  that are contained in both time periods, \code{death1}
  and \code{death2} determine the fraction of records of
  the first time period that are absent in the second,
  where \code{death1} takes the first time period at base,
  and \code{death2} the second. \code{birth} determines the
  fraction of records that are only contained in the second
  time period.
}

