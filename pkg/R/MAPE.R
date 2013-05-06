#' (Symmetric) mean absolute percentage error
#'
#' The function MAPE is an implemention of the mean absolute percentage error, and sMAPE of the symmetric mean absolute percentage error (version (3) in \url{http://www.monashforecasting.com/index.php?title=SMAPE})
#'
#' @name MAPE
#' @param actual the vector of actual values
#' @param forecast the vector of forecasted values
#' @return the (s)MAPE value
#' @example ../examples/MAPE.R
#' @export
MAPE <- function(actual, forecast) {
    
    if (!is.vector(actual)) stop("actual is not a vector")
    if (!is.vector(forecast)) stop("forecast is not a vector")
    
    if (length(actual) != length(forecast)) stop("actual and forecast have different lengths")
    
    n <- length(actual)
    
    res <- (100 / n) * sum((actual-forecast)/actual)
    res
}



#' @rdname MAPE
#' @aliases sMAPE
#' @export
sMAPE <- function(actual, forecast) {
    if (!is.vector(actual)) stop("actual is not a vector")
    if (!is.vector(forecast)) stop("forecast is not a vector")
    
    if (length(actual) != length(forecast)) stop("actual and forecast have different lengths")
    
    n <- length(actual)

    res <- (1/n) * sum(abs(actual-forecast) / ((abs(actual)+abs(forecast))/2))
    res
}