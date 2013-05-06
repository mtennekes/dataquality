\name{changed_value}
\alias{changed_value}
\alias{unchanged_value}
\title{Checks the number of (un)changed values between two time periods of a data source}
\usage{
  changed_value(source_t1.data, source_t2.data,
    source_t1.keys = keys, source_t2.keys = source_t1.keys,
    keys, variable_t1 = variable,
    variable_t2 = variable_t1, variable,
    percentage = FALSE, ...)

  unchanged_value(source_t1.data, source_t2.data,
    source_t1.keys = keys, source_t2.keys = source_t1.keys,
    keys, variable_t1 = variable,
    variable_t2 = variable_t1, variable,
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

  \item{variable_t1}{name of the checked variable in
  \code{source_t1.data}}

  \item{variable_t2}{name of the checked variable in
  \code{source_t2.data}}

  \item{variable}{name of the checked variable in both data
  sources}

  \item{percentage}{should percentage number be returned?
  (the default value is \code{FALSE}, which means that a
  fraction is returned)}

  \item{...}{arguments passed on other functions (unused)}
}
\value{
  a fraction (or percentage)
}
\description{
  The function \code{changed_value} determines the number
  of changed values of a variable between two time periods
  of a data source.
}

