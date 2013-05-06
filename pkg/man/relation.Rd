\name{relation}
\alias{relation}
\title{Describes the relationship between two variables in a data source.}
\usage{
  relation(data, var1, var2, draw = FALSE,
    graph.lowerbound.numberV1perV2 = NULL,
    graph.lowerbound.numberV2perV1 = NULL)
}
\arguments{
  \item{data}{data source (data.frame)}

  \item{var1}{name of the first variable of \code{data}}

  \item{var2}{name of the second variable of \code{data}}

  \item{draw}{logical that determines whether to plot a
  graph}

  \item{graph.lowerbound.numberV1perV2}{lowerbound for the
  number of units \code{var1} per unit \code{var2} that are
  plot in a graph}

  \item{graph.lowerbound.numberV2perV1}{lowerbound for the
  number of units \code{var2} per unit \code{var1} that are
  plot in a graph}
}
\value{
  the descriptions are silently returned in a list
}
\description{
  Describes the relationship between two variables in a
  data source. Both 1-to-n and m-to-1 relations are
  explored.
}

