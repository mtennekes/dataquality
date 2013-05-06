#' Describes the redundancy between objects.
#' 
#' Describes the number of duplicated values per selected variable, and the number of duplicated objects (i.e. objects that have the same values on all selected variables).
#'
#' @param data data source (data.frame)
#' @param vars names of the variables \code{data} that are checked on redundancy
#' @param percentages logical that determines whether to provide the output in percentages
#' @return the descriptions are silently returned in a list
#' @export
redundancy <- function(data, vars=names(data), percentages=FALSE) {
	data <- data[, vars, drop=FALSE]
	missings <- sapply(data, function(x)sum(is.na(x)))
	
	
	dups <- sapply(data, function(x)duplicated(x)&!is.na(x))

	nduplicated <- apply(dups, MARGIN=2, FUN=sum)
	
	
	tab <- data.frame(values=nrow(data)-missings,
			   missing=missings,
			   unique=nrow(data)-missings-nduplicated,
			   duplicated=nduplicated)

	if (percentages) {
		tab <- round(tab/nrow(data) * 100, 2)
		cat("Summary of values per variable (in percentages)\n")
	} else {
		cat("Summary of values per variable\n")
	}
	
	print(tab)
	
	if (length(vars)==1) {
         res <- list(variables=tab)
	} else {
    	cat("------------------------------\n")
    	
    	dup <- sum(duplicated(data))
    	
    	complete_df <- sum(apply(data, MARGIN=1, function(x)all(!is.na(x))))
    	missings_df <- sum(apply(data, MARGIN=1, function(x)all(is.na(x))))
    	
    	tab2 <- c(total=nrow(data),
    			  complete=complete_df,
    			  partly_missing=nrow(data)-complete_df-missings_df,
    			  completely_missing=missings_df,
    			  unique=nrow(data)-dup, 
    			  duplicated=dup)
    	
    	
    	if (percentages) {
    		tab2 <- round(tab2 / nrow(data) * 100, 2)
    		cat("Summary of objects regarding all selected variables (in percentages)\n")
    	} else {
    		cat("Summary of objects regarding all selected variables\n")
    	}
    
    	print(tab2)
    	res <- list(variables=tab, total=tab2)
	}
    
    
	invisible(res)
}