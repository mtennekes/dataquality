\name{tableplot}
\alias{tableplot}
\title{Create a tableplot}
\usage{
  tableplot(...)
}
\arguments{
  \item{...}{arguments passed to
  \code{\link[tabplot]{tableplot}}}
}
\value{
  See \code{\link[tabplot]{tableplot}}
}
\description{
  A tableplot is a visualisation of (large) multivariate
  datasets. Each column represents a variable and each row
  bin is an aggregate of a certain number of records. For
  numeric variables, a bar chart of the mean values is
  depicted. For categorical variables, a stacked bar chart
  is depicted of the proportions of categories. Missing
  values are taken into account. Also supports large ffdf
  datasets from the ff package.
}

