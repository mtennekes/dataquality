\name{visualise}
\alias{visualise}
\title{Visualise one or two data sources.}
\usage{
  visualise(data1, data2 = NULL,
    vars1 = intersect(names(data1), names(data2)),
    vars2 = vars1, showAll = TRUE,
    colours = c("blue", "green"), useRelative = FALSE)
}
\arguments{
  \item{data1}{first data source (data.frame)}

  \item{data2}{second data source (data.frame)}

  \item{vars1}{names of variables of which grouped
  histograms are plotted. Should correspond one to one to
  \code{vars2}}

  \item{vars2}{names of variables of which grouped
  histograms are plotted. Should correspond one to one to
  \code{vars1}}

  \item{showAll}{boolean that specifies whether all
  variables are plotted, or only the variables specified in
  \code{vars1} and \code{vars2}.}

  \item{colours}{vector of two colours: one for the first
  and one for the second data source}

  \item{useRelative}{boolean that specifies whether
  histogram values (vertical axis) are relative or absolute
  values.}
}
\description{
  Visualise one or two data sources. A histrogram is
  plotted for each numeric variable and a bar chart for
  each categorical variable. For each variable that is
  available in both sources, a grouped histogram/bar chart
  is plotted in which the distributions within both sources
  can be compared. Either absolute or relative values can
  be used.
}
\details{
  If all values in a categorical variable are unique, then
  the text "Unique values" is given. If a categorical
  variable contains more than 30 categories (which may be
  the case with identification variables), then the text
  "Too many categories" is given. Note that in the latter
  case, the values are not unique (for otherwise "Unique
  values" is given).
}

