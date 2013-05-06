#' Checks the format of one or more variables in a dataset.
#' 
#' Checks the format of one or more variables in a dataset. For each variable, a summary is given how of many values satisfy the specified pattern.
#'
#' @param data data source (data.frame)
#' @param var name(s) of the variable(s) of \code{data}
#' @param pattern character string containing a \link{regular expression}
#' @param n maximum number of invalid values that are displayed
#' @return the results are silently returned in a list
#' @export
checkFormat <- function(data, var, pattern, n=100) {

    if (!all(var %in% names(data))) stop("Invalid variable name(s)")
    
    result <- list()
    for (v in var) {
        vec <- as.character(na.omit(data[[v]]))
        
        res <- grep(pattern, x=vec, invert=TRUE)
        
        nomatch <- vec[res]
        
        missings <- nrow(data)-length(vec)
        missingsP <- round(missings/nrow(data)*100,2)
        
        valids <- length(vec)-length(nomatch)
        validsP <- round(valids/nrow(data)*100,2)
        
        invalids <- length(nomatch)
        invalidsP <- round(invalids/nrow(data)*100,2)
        
        cat("-------------------")
        cat("Variable", v, "\n")
        cat("Number of objects:", nrow(data), "\n")
        cat("Number of missing values:", missings, 
            paste("(", missingsP, "%)\n", sep=""))
        cat("Pattern:", pattern, "\n")
        cat("Number of valid values:", valids,
            paste("(", validsP, "%)\n", sep=""))
        cat("Number of invalid values:", invalids,
            paste("(", invalidsP, "%)\n", sep=""))

        if (length(nomatch)>0)
            cat("Invalid values:", head(nomatch,n), ifelse(length(nomatch)>n, "...\n", "\n"))
        
        result[[v]] <- list(n_missings=missings,
                            perc_missings=missingsP,
                            n_valid=valids,
                            perc_valid=validsP,
                            n_invalid=invalids,
                            perc_invalid=invalidsP,
                            invalid_valies=nomatch)
    }
    invisible(result)
}