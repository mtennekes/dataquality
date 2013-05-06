\name{compare}
\alias{compare}
\title{Visually compare two data sources}
\usage{
  compare(data1, data2,
    key1 = intersect(names(data1), names(data2))[1],
    key2 = key1,
    vars1 = setdiff(intersect(names(data1), names(data2)), key1),
    vars2 = vars1)
}
\arguments{
  \item{data1}{first data source (data.frame)}

  \item{data2}{second data source (data.frame)}

  \item{key1}{name(s) of key variable(s) from \code{data1}.
  Should correspond one to one to \code{key2}.}

  \item{key2}{name(s) of key variable(s) from \code{data2}.
  Should correspond one to one to \code{key1}.}

  \item{vars1}{name(s) of the variable(s) from \code{data1}
  that are visually compared (one to one) to \code{vars2}.}

  \item{vars2}{name(s) of the variable(s) from \code{data2}
  that are visually compared (one to one) to \code{vars1}.}
}
\description{
  Visually compare two data sources. The data sources are
  joined by one or more keys. Only key values that occur in
  both sources are taken into account (where duplicated are
  omitted). For each numerical variable, a scatter plot
  with box plots is plotted, and for each categorical
  variable a fluctuation plot. Missing values are taken
  into account.
}

