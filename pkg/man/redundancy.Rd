\name{redundancy}
\alias{redundancy}
\title{Describes the redundancy between objects.}
\usage{
  redundancy(data, vars = names(data), percentages = FALSE)
}
\arguments{
  \item{data}{data source (data.frame)}

  \item{vars}{names of the variables \code{data} that are
  checked on redundancy}

  \item{percentages}{logical that determines whether to
  provide the output in percentages}
}
\value{
  the descriptions are silently returned in a list
}
\description{
  Describes the number of duplicated values per selected
  variable, and the number of duplicated objects (i.e.
  objects that have the same values on all selected
  variables).
}

