##Generic functions: way to convert key input to column-numbers
fix.keys <- function(keys, df)
{
    ## fix up 'keys' to be a valid set of cols by number: 0 is row.names
    if(is.null(keys)) keys <- numeric(0L)        
    keys <- as.vector(keys)
    nc <- ncol(df)
    if(is.character(keys))  ## make sure row.names can be included as key
        keys <- match(keys, c("row.names", names(df))) - 1L
    else if(is.numeric(keys)) {
        if(any(keys < 0L) || any(keys > nc))
            stop("'keys' must match numbers of columns")
    } else if(is.logical(keys)) {
        if(length(keys) != nc) stop("'keys' must match number of columns")
        keys <- seq_along(keys)[keys] ##check if this works
    } else stop("'keys' must specify column(s) as numbers, names or logical")
    if(any(is.na(keys))) stop("'keys' must specify valid column(s)")

    ## return unique keys (not sorted)
    unique(keys)
}

##Generic functions: way to compare the value of 2 variables and cope with NA
compareNA <- function(v1,v2) 
{
    ## This function returns TRUE wherever elements are the same, including NA's,
    ## and false everywhere else.
    same <- (v1 == v2)  |  (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE  ## return FALSE When a single NA is included (otherwise NA is returned)
    return(same)
}

##Generic function: find number of duplicated keys
check_duplicated_keys <- function(id, label) { 
	## identify duplicated keys
	dupl <- sum(duplicated(id))
	
	if (dupl==1) {
		cat(label, "contain", dupl, "duplicated record. This record is omitted.\n")
	} else if (dupl!=0) {
		cat(label, "contain", dupl, "duplicated records. These records are omitted.\n")
	}
	invisible()
}

## Generic functions: way to compare records in 2 files, to calculate coverage or non-coverage *(over and under)
XinY <- function(source.data, reference.data,  source.keys = keys, reference.keys = source.keys, keys = intersect(names(source.data), names(reference.data)),
             missing = FALSE, incomparables = NULL, warning_duplicates=TRUE,
             ...)
{
    ## total records in data sources
    nsrc <- nrow(source.data <- as.data.frame(source.data))
    nref <- nrow(reference.data <- as.data.frame(reference.data))
    
    ## Get columnnumbers of keys provided
    source.keys <- fix.keys(source.keys, source.data)   
    reference.keys <- fix.keys(reference.keys, reference.data)
  
    ## Make sure keys can be compared     
    if(length(source.keys) != length(reference.keys))
        stop("'source keys' and 'reference keys' specify different numbers of columns")
    if(length(source.keys) == 0L) {
        stop("no columns to match on")        
    }
    else {
        if(any(source.keys == 0L)) {    ##Use row names when 0 colum is selected
            source.data <- cbind(Row.names = I(row.names(source.data)), source.data)
            source.keys <- source.keys + 1L
        }
        if(any(reference.keys == 0L)) { ##Use row names when 0 colum is selected
            reference.data <- cbind(Row.names = I(row.names(reference.data)), reference.data)
            reference.keys <- reference.keys + 1L
        }
        ## create keys vectors from 'keys' provided:
        if(length(source.keys) == 1L) {                  # for a single key, to be faster
            bsrc <- source.data[, source.keys]; if(is.factor(bsrc)) bsrc <- as.character(bsrc)
            bref <- reference.data[, reference.keys]; if(is.factor(bref)) bref <- as.character(bref)
        } else {
            ## For multiple keys, select relevant columns (drop the rest) and combine
            bsrc <- do.call("paste", c(rbind(source.data[, source.keys, drop=FALSE]), sep = "_"))
            bref <- do.call("paste", c(rbind(reference.data[, reference.keys, drop=FALSE]), sep = "_"))                    
        }
        
        ## check on duplicated keys
        if (warning_duplicates) check_duplicated_keys(bsrc, "Source data")
        if (warning_duplicates) check_duplicated_keys(bref, "Reference data")
        
        ## Check key comparison method to be used
        if (missing) {
            ##records <- nrow(source.data[comm == 0,]) ##report number of unique non-matches
            records <- length(setdiff(bsrc, bref))
        } else {
            ##records <- nrow(source.data[comm > 0,])  ##report number of unique key matches
            records <- length(intersect(bsrc, bref))
        }
    }
       
    ##check if keys names in source and reference are exactly alike
    if (all(names(source.data[source.keys]) == names(reference.data[reference.keys]))) {
      keys.names <- names(source.data[source.keys])
    } else {  ## report keys used, source first with corresponding reference key in brackets
      keys.names <- paste(as.character(names(source.data[, source.keys, drop=FALSE])), "(", as.character(names(reference.data[, reference.keys, drop=FALSE])), ")", sep = " " )
    }
         
    ## create result list 
    result <- list(keys = keys.names, no.records = records, no.source = length(unique(bsrc)), no.reference = length(unique(bref)) )
    
    return(result)   
}

##Generic functions: way to match records in 2 files and compare value of selected variable
Xchanged2Y <- function(source_t1.data, source_t2.data, source_t1.keys = keys, source_t2.keys = source_t1.keys, keys,
              variable_t1 = variable, variable_t2 = variable_t1, variable, changed = TRUE, incomparables = NULL,
              ...)
{
    
    ## determine number of records with same keys in 2 sources for which the value of a variable provided has changed  
    if(length(variable_t1) == 0L)
      stop("A variable to compare must be provided")
    if(length(variable_t1) > 1L)
      stop("A single variable to compare must be provided")
    if(length(variable_t2) == 0L)
      stop("A variable to compare must be provided")
    if(length(variable_t2) > 1L)
      stop("A single variable to compare must be provided")
     
    ## total records in data sources
    nsrc <- nrow(source_t1.data <- as.data.frame(source_t1.data))
    nref <- nrow(source_t2.data <- as.data.frame(source_t2.data))
    
    ## Get columnnumbers of variable provided
    variable_t1 <- fix.keys(variable_t1, source_t1.data)   
    if(any(source_t1.keys == "row.names")) {variable_t1 <- variable_t1 + 1L} ##adjust for effect of using row.names
    variable_t2 <- fix.keys(variable_t2, source_t2.data)
    if(any(source_t2.keys == "row.names")) {variable_t2 <- variable_t2 + 1L} ##adjust for effect of using row.names
    ##Laatse doet nog raar bij row.names 
    ## Get columnnumbers of keys provided
    source_t1.keys <- fix.keys(source_t1.keys, source_t1.data)   
    source_t2.keys <- fix.keys(source_t2.keys, source_t2.data)
     
    ## Check if keys and variable overlap
    if(any(source_t1.keys == variable_t1))
      stop("source 1 keys and variable overlap")
    if(any(source_t2.keys == variable_t2))
      stop("source 2 keys and variable overlap")
    ## Make sure keys can be compared     
    if(length(source_t1.keys) != length(source_t2.keys))
        stop("'source 1 keys' and 'source 2 keys' specify different numbers of columns")
    if(length(source_t1.keys) == 0L) {
        stop("no columns to match on")        
    }
    else {
        if(any(source_t1.keys == 0L)) {    ##Use row names when 0 colum is selected
            source_t1.data <- cbind(Row.names = I(row.names(source_t1.data)), source_t1.data)
            source_t1.keys <- source_t1.keys + 1L
        }
        if(any(source_t2.keys == 0L)) { ##Use row names when 0 colum is selected
            source_t2.data <- cbind(Row.names = I(row.names(source_t2.data)), source_t2.data)
            source_t2.keys <- source_t2.keys + 1L
        }
        ## create keys vectors from 'keys' provided:
        if(length(source_t1.keys) == 1L) {                  # for a single key, to be faster
            bsrc <- source_t1.data[, source_t1.keys]; if(is.factor(bsrc)) bsrc <- as.character(bsrc)
            bref <- source_t2.data[, source_t2.keys]; if(is.factor(bref)) bref <- as.character(bref)
        } else {
            ## For multiple keys, select relevant columns (drop the rest) and combine
            bsrc <- do.call("paste", c(rbind(source_t1.data[, source_t1.keys, drop=FALSE]), sep = "_"))
            bref <- do.call("paste", c(rbind(source_t2.data[, source_t2.keys, drop=FALSE]), sep = "_"))                    
        }
        
        ## check on duplicated keys
        check_duplicated_keys(bsrc, "Source data")
        check_duplicated_keys(bref, "Reference data")
        
        ## match on keys, zero is used when ? record does not match, comm contains for each 
        comm <- match(bsrc, bref, 0L)
        
        ## remove duplicated matches
        comm[duplicated(comm)] <- 0
        
        
        ##compare variable values for linked records
        commPos <- comm!=0
        matched <- sum(commPos)
        differ <- sum(!compareNA(source_t1.data[which(commPos), variable_t1], source_t2.data[comm[commPos], variable_t2]))
        
        
        # old code:
        #matched <- 0
        #differ <- 0
        #for(i in 1:length(comm)) {
          ## only when a record is linked
        #  if(comm[i] != 0) {           
        #      matched <- matched + 1 ##count number of matched records
              ##check differences, i.e  if variable value of matched source 1 record with matched source 2 record differs, and deal with NA (NA == NA is TRUE)
        #      if (!compareNA(source_t1.data[i, variable_t1], source_t2.data[comm[i], variable_t2])) {
        #        differ <- differ + 1 ##check if values have changed
        #      }
        #  }
        #} 
        
        ## check if unchanged values have to be reported : number matched minus changed
        if(!changed) { 
          differ <- (matched - differ)  ##Use matched records as set to compare to
        }          
    }   
    
    ##check if keys names in source1 and source2 are exactly alike
    if (all(names(source_t1.data[source_t1.keys]) == names(source_t2.data[source_t2.keys]))) {
      keys <- names(source_t1.data[source_t1.keys])
    } else {  ## report keys used, source1 first with corresponding source2 key in brackets
      keys <- paste(as.character(names(source_t1.data[, source_t1.keys, drop=FALSE])), "(", as.character(names(source_t2.data[, source_t2.keys, drop=FALSE])), ")", sep = " " )
    }
    
    ##check if variable names in source1 and source2 are exactly alike
    if (all(names(source_t1.data[variable_t1]) == names(source_t2.data[variable_t2]))) {
      var <- names(source_t1.data[variable_t1])
    } else {  ## report variable names used, source1 first with corresponding source2 key in brackets
      var <- paste(as.character(names(source_t1.data[variable_t1])), "(", as.character(names(source_t2.data[variable_t2])), ")", sep = " " )
    }    
    
    ## create result list 
    result <- list(keys = keys, variable = var, no.records = differ, no.matched = matched, no.total = length(comm) )
    
    return(result)
}

#' Determines the coverage of a data source with respect to a reference dataset
#'
#' Determines the coverage of a data source with respect to a reference dataset. The function coverage determines the number, fraction, and percentage of matching records. The functions overcoverage and undercoverage respectively determines the oves- respectively undercoverage in terms of numbers, fractions and percentages of non-matching records.
#'
#' @name coverage
#' @param source.data source data (data.frame)
#' @param reference.data reference data (data.frame)
#' @param source.keys key variable(s) of \code{source.data}. Either a character vector of variable names, a numeric vector of column indices, or a logical vector indicating which columns to select.
#' @param reference.keys key variable(s) of \code{reference.data}. Either a character vector of variable names, a numeric vector of column indices, or a logical vector indicating which columns to select.
#' @param keys common key variable(s) of \code{source.data} and \code{reference.data}.
#' @param percentage logical that determines whether to return the coverge result in percentages
#' @param ... arguments passed on to other functions (unused)
#' @return coverage/overcoverage/undercoverage fraction (or percentage)
#' @export 
coverage <- function(source.data, reference.data, source.keys = keys, reference.keys = source.keys, keys = intersect(names(source.data), names(reference.data)),
              percentage =  FALSE, ...)
{
    ## determine number of records with identical keys in both sources with source 1 as base
    result <- XinY(source.data, reference.data, source.keys, reference.keys, keys, missing = FALSE)

    ## calculate fraction matching with source as base
    if (result$no.source > 0) {
            res <- (result$no.records/result$no.source)        
    } else {
           res <- 0 
    }   
        
    ## report result
    cat("Keys used:", result$keys, "\n")
    cat("Number of matching records:", result$no.records,"(of", result$no.source, ")\n") 
    cat("Fraction of matching records:", res, ", (", res*100, "%)\n")
    
    ## check if percentage needs to be returned
    if (percentage) res <- res*100

    invisible(res)    
}

#' @rdname coverage
#' @aliases overcoverage
#' @export
overcoverage <- function(source.data, reference.data, source.keys = keys, reference.keys = source.keys, keys = intersect(names(source.data), names(reference.data)),
              percentage =  FALSE, ...)
{
    
    ## determine the number of records with different keys in source 1 compared to source  with source 1 as base
    result <- XinY(source.data, reference.data, source.keys, reference.keys, keys, missing = TRUE)

    ## calculate fraction non-matching with source as base
    if (result$no.source > 0) {
            res <- (result$no.records/result$no.source)        
    } else {
            res <- 0 
    }
          
    ## report result
    cat("Keys used:", result$keys, "\n")
    cat("Number of non-matching records:", result$no.records,"(of", result$no.source, ")\n") 
    cat("Fraction of non-matching records:", res, ", (", res*100, "%)\n")
    
    ## check if percentage needs to be returned
    if (percentage) res <- res*100

    invisible(res)    
}

#' @rdname coverage
#' @aliases undercoverage
#' @export
undercoverage <- function(source.data, reference.data, source.keys = keys, reference.keys = source.keys, keys = intersect(names(source.data), names(reference.data)),
              percentage =  FALSE, ...)
{
    ## determine the number of records with different keys in source 2 compared to source 1, with source 1 as base 
    result <- XinY(reference.data, source.data, reference.keys, source.keys, keys, missing = TRUE)
    
    ## calculate fraction non-matching with source as base (-> which is the orginial reference file)
    if (result$no.source > 0) {
          res <- (result$no.records/result$no.source)        
    } else {
          res <- 0 
    }
            
    ## report result
    cat("Keys used:", result$keys, "\n")
    cat("Number of non-matching records:", result$no.records,"(of", result$no.source, ")\n") 
    cat("Fraction of non-matching records:", res, ", (", res*100, "%)\n")
    
    ## check if percentage needs to be returned
    if (percentage) res <- res*100

    invisible(res)
}


#' Functions to determine population dynamics.
#'
#' These functions determine the population dynamics between two time periods of a data source. \code{change} provides the fraction of changed values, and also a summary of new and removed records, \code{change_fast} is a fast equavalent that only provides the fraction of changed values. \code{alive} provides the fraction of records that are contained in both time periods, \code{death1} and \code{death2} determine the fraction of records of the first time period that are absent in the second, where \code{death1} takes the first time period at base, and \code{death2} the second. \code{birth} determines the fraction of records that are only contained in the second time period.
#'
#' @name change
#' @param source_t1.data data source at time period t1 (data.frame)
#' @param source_t2.data data source at time period t2 (data.frame)
#' @param source_t1.keys key variable(s) of \code{source_t1.data}. A character vector of variable names, a vector of column indices, or a logical vector to select columns. 
#' @param source_t2.keys key variable(s) of \code{source_t2.data}. A character vector of variable names, a vector of column indices, or a logical vector to select columns.
#' @param keys common key variable(s).  A character vector of variable names that are contained in both \code{source_t1.data} and \code{source_t2.data}.
#' @param percentage should percentage number be returned? (the default value is \code{FALSE}, which means that a fraction is returned)
#' @param ... arguments passed on other functions (unused)
#' @return a fraction (or percentage)
#' @export
change <- function(source_t1.data, source_t2.data, source_t1.keys = keys, source_t2.keys = source_t1.keys, keys = intersect(names(source_t1.data), names(source_t2.data)),
                   percentage =  FALSE, ...)
{
    ## MOET TOCH VEEL HANDIGER KUNNEN, door vergelijken totaal unique records van bronnen
    
    ## determine the number of records with new keys in source 2 compared to source 1 with source 2 as base
    birth_result <- XinY(source_t2.data, source_t1.data, source_t2.keys, source_t1.keys, keys, missing = TRUE)
    
    ## determine number of records with keys present in source 1 and absent in source 2 with source 2 as base
    death2_result <- XinY(source_t1.data, source_t2.data, source_t1.keys, source_t2.keys, keys, missing = TRUE, warning_duplicates=FALSE)
    
    ## calculate change in objects
    change_in_records = birth_result$no.records - death2_result$no.records
    
    ## calculate fraction change in objects compared to source t2 as base
    if (death2_result$no.reference > 0) {
        res <- (change_in_records/death2_result$no.reference)        
    } else {
        res <- 0 
    }
    
    ## report result
    cat("Keys used:", death2_result$keys, "\n")
    cat("Number of new records:", birth_result$no.records, ", number of removed records:", death2_result$no.records, "\n")
    cat("Overall change in records:", change_in_records,"(of", death2_result$no.reference, ")\n") 
    cat("Fraction of change in records:", res, ", (", res*100, "%)\n")
    
    ## check if percentage needs to be returned
    if (percentage) res <- res*100
    
    invisible(res)
}

#' @rdname change
#' @aliases change change_fast
#' @export
change_fast <- function(source_t1.data, source_t2.data, source_t1.keys = keys, source_t2.keys = source_t1.keys, keys = intersect(names(source_t1.data), names(source_t2.data)),
                        percentage =  FALSE, ...)
{
    ## Fast way to determine overall change in number of records, with the downside that birth and death details are lost
    
    ## determine number of records with keys present in source 1 and absent in source 2 with source 1 as base
    ## method is only used to get the absolute nummer of unique records in source1 ad source2
    result <- XinY(source_t1.data, source_t2.data, source_t1.keys, source_t2.keys, keys, missing = TRUE)
    
    ## calculate total change in unique objects between sources
    change_in_records = result$no.reference - result$no.source
    
    ## calculate fraction change in objects compared to source t2 as base
    if (result$no.reference > 0) {
        res <- (change_in_records/result$no.reference)        
    } else {
        res <- 0 
    }
    
    ## report result
    cat("Keys used:", result$keys, "\n")
    cat("Overall change in records:", change_in_records,"(of", result$no.reference, ")\n") 
    cat("Fraction of change in records:", res, ", (", res*100, "%)\n")
    
    ## check if percentage needs to be returned
    if (percentage) res <- res*100
    
    invisible(res)
}

#' @rdname change
#' @aliases alive
#' @export
alive <- function(source_t1.data, source_t2.data, source_t1.keys = keys, source_t2.keys = source_t1.keys, keys = intersect(names(source_t1.data), names(source_t2.data)),
              percentage =  FALSE, ...)
{
     ## determine number of records with identical keys in two sources with source 2 as base
     result <- XinY(source_t1.data, source_t2.data, source_t1.keys, source_t2.keys, keys, missing = FALSE)
    
    ## calculate fraction matching records with second file as base
    if (result$no.reference > 0) {
        res <- (result$no.records/result$no.reference)        
    } else {
        res <- 0 
    }
          
    ## report result
    cat("Keys used:", result$keys, "\n")
    cat("Number of remaining records:", result$no.records,"(of", result$no.reference, ")\n") 
    cat("Fraction of remaining records:", res, ", (", res*100, "%)\n")
    
    ## check if percentage needs to be returned
    if (percentage) res <- res*100

    invisible(res)
}


#' @rdname change
#' @aliases death1
#' @export
death1 <- function(source_t1.data, source_t2.data, source_t1.keys = keys, source_t2.keys = source_t1.keys, keys = intersect(names(source_t1.data), names(source_t2.data)),
              percentage =  FALSE, ...)
{
    ## determine number of records with keys present in source 1 and absent in source 2 with source 1 as base
    result <- XinY(source_t1.data, source_t2.data, source_t1.keys, source_t2.keys, keys, missing = TRUE)

    ## calculate fraction non-matching records with first source as base
    if (result$no.source > 0) {
        res <- (result$no.records/result$no.source)        
    } else {
        res <- 0 
    }
          
    ## report result
    cat("Keys used:", result$keys, "\n")
    cat("Number of removed records:", result$no.records,"(of", result$no.source, ")\n") 
    cat("Fraction of removed records:", res, ", (", res*100, "%)\n")
    
    ## check if percentage needs to be returned
    if (percentage) res <- res*100

    invisible(res)
}

#' @rdname change
#' @aliases death2
#' @export
death2 <- function(source_t1.data, source_t2.data, source_t1.keys = keys, source_t2.keys = source_t1.keys, keys = intersect(names(source_t1.data), names(source_t2.data)),
              percentage =  FALSE, ...)
{
    ## determine the number of records with keys present in source 1 and absent in source 2 with source 2 as base
    result <- XinY(source_t1.data, source_t2.data, source_t1.keys, source_t2.keys, keys, missing = TRUE)

    ## calculate fraction non-matching with second file as base
    if (result$no.reference > 0) {
        res <- (result$no.records/result$no.reference)        
    } else {
        res <- 0 
    }
            
    ## report result
    cat("Keys used:", result$keys, "\n")
    cat("Number of removed records:", result$no.records,"(of", result$no.reference, ")\n") 
    cat("Fraction of removed records:", res, ", (", res*100, "%)\n")
    
    ## check if percentage needs to be returned
    if (percentage) res <- res*100

    invisible(res)  
}


#' @rdname change
#' @aliases birth
#' @export
birth <- function(source_t1.data, source_t2.data, source_t1.keys = keys, source_t2.keys = source_t1.keys, keys = intersect(names(source_t1.data), names(source_t2.data)),
              percentage =  FALSE, ...)
{
    ## determine the number of records with new keys in source 2 compared to source 1 with source 2 as base
    result <- XinY(source_t2.data, source_t1.data, source_t2.keys, source_t1.keys, keys, missing = TRUE)
    
    ## calculate fraction new records in source t2 with this file as base (-> first file provided: source )
    if (result$no.source > 0) {
          res <- (result$no.records/result$no.source)        
    } else {
          res <- 0 
    }
        
    ## report result
    cat("Keys used:", result$keys, "\n")
    cat("Number of new records:", result$no.records,"(of", result$no.source, ")\n") 
    cat("Fraction of new records:", res, ", (", res*100, "%)\n")
    
    ## check if percentage needs to be returned
    if (percentage) res <- res*100

    invisible(res)
}


#' Checks the number of (un)changed values between two time periods of a data source
#'
#' The function \code{changed_value} determines the number of changed values of a variable between two time periods of a data source.
#'
#' @name changed_value 
#' @param source_t1.data data source at time period t1 (data.frame)
#' @param source_t2.data data source at time period t2 (data.frame)
#' @param source_t1.keys key variable(s) of \code{source_t1.data}. A character vector of variable names, a vector of column indices, or a logical vector to select columns.
#' @param source_t2.keys key variable(s) of \code{source_t2.data}. A character vector of variable names, a vector of column indices, or a logical vector to select columns.
#' @param keys common key variable(s).  A character vector of variable names that are contained in both \code{source_t1.data} and \code{source_t2.data}.
#' @param variable_t1 name of the checked variable in \code{source_t1.data}
#' @param variable_t2 name of the checked variable in \code{source_t2.data}
#' @param variable name of the checked variable in both data sources
#' @param percentage should percentage number be returned? (the default value is \code{FALSE}, which means that a fraction is returned)
#' @param ... arguments passed on other functions (unused)
#' @return a fraction (or percentage)
#' @export
changed_value <- function(source_t1.data, source_t2.data, source_t1.keys = keys, source_t2.keys = source_t1.keys, keys,
                          variable_t1 = variable, variable_t2 = variable_t1, variable, percentage = FALSE, ...)
{
    ## determine the number of records matched that have not chend tha value of variable provided
    result <- Xchanged2Y(source_t1.data, source_t2.data, source_t1.keys, source_t2.keys, keys, variable_t1, variable_t2, variable, changed = TRUE)
    
    ## calculate fraction matched records in source 1 and t2 of which the variable value is not changed
    if (result$no.matched > 0) {
        res <- (result$no.records/result$no.matched)        
    } else {
        res <- 0 
    }
    
    ## report result
    cat("Keys used:", result$keys, "\n")
    cat("Variable compared:", result$variable, "\n")
    cat("Number of records changed:", result$no.records,"(of", result$no.matched, " aligned records)\n") 
    cat("Fraction of records changed:", res, ", (", res*100, "%)\n")
    
    ## check if percentage needs to be returned
    if (percentage) res <- res*100
    
    invisible(res)
}

#' @rdname changed_value
#' @aliases changed_value unchanged_value
#' @export
unchanged_value <- function(source_t1.data, source_t2.data, source_t1.keys = keys, source_t2.keys = source_t1.keys, keys,
              variable_t1 = variable, variable_t2 = variable_t1, variable, percentage = FALSE, ...)
{
    ## determine the number of records matched that have not chend tha value of variable provided
    result <- Xchanged2Y(source_t1.data, source_t2.data, source_t1.keys, source_t2.keys, keys, variable_t1, variable_t2, variable, changed = FALSE)
    
    ## calculate fraction matched records in source 1 and t2 of which the variable value is not changed
    if (result$no.matched > 0) {
          res <- (result$no.records/result$no.matched)        
    } else {
          res <- 0 
    }
    
    ## report result
    cat("Keys used:", result$keys, "\n")
    cat("Variable compared:", result$variable, "\n")
    cat("Number of records unchanged:", result$no.records,"(of", result$no.matched, " aligned records)\n") 
    cat("Fraction of records unchanged:", res, ", (", res*100, "%)\n")
    
    ## check if percentage needs to be returned
    if (percentage) res <- res*100

    invisible(res)
}



