#' Cramer's V
#' 
#' Implementation of Cramer's V
#' 
#' @param source1.data first data source (data.frame)
#' @param source2.data second data source (data.frame)
#' @param source1.keys keys of \code{source1.data}
#' @param source2.keys keys of \code{source2.data}
#' @param source1.variable name of the categorical variable (from \code{source1.data})
#' @param source2.variable name of the categorical variable (from \code{source2.data})
#' @return Cramer's V
#' @export
cramerV.test <- function(source1.data, source2.data, source1.keys, source2.keys, source1.variable, source2.variable) {
    if (!source1.variable %in% names(source1.data)) stop("Invalid var1")
    if (!source2.variable %in% names(source2.data)) stop("Invalid var2")
    
    # remove duplicates
    source1.data <- source1.data[!duplicated(source1.data[, source1.keys]), ]
    source2.data <- source2.data[!duplicated(source2.data[, source2.keys]), ]
    
    data <- merge(source1.data, source2.data, by.x=source1.keys, by.y=source2.keys)
    var1 <- ifelse(source1.variable %in% names(source2.data), paste(source1.variable, "x", sep="."), source1.variable)
    var2 <- ifelse(source2.variable %in% names(source1.data), paste(source2.variable, "y", sep="."), source2.variable)
    
    x <- data[[var1]]
    y <- data[[var2]]
    CV = sqrt(chisq.test(x, y, correct=FALSE)$statistic /
        (length(x) * (min(length(unique(x)),length(unique(y))) - 1)))
    cat("Cramer V / Phi:", CV)
    invisible(CV)
}
