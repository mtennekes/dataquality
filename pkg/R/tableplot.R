#' Create a tableplot
#'
#' A tableplot is a visualisation of (large) multivariate datasets. Each column represents a variable and each row bin is an aggregate of a certain number of records. For numeric variables, a bar chart of the mean values is depicted. For categorical variables, a stacked bar chart is depicted of the proportions of categories. Missing values are taken into account. Also supports large ffdf datasets from the ff package.
#'
#' @param ... arguments passed to \code{\link[tabplot]{tableplot}}
#' @return See \code{\link[tabplot]{tableplot}}
#' @export
tableplot <- function(...) {
	tabplot::tableplot(...)
}

#' GUI for creating tableplots
#'
#' GUI for creating tableplots
#'
#' @param ... arguments passed to \code{\link[tabplot]{itableplot}}
#' @return See \code{\link[tabplot]{itableplot}}
#' @export
itableplot <- function(...) {
	tabplot::itableplot(...)
}
