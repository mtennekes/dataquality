#' Visualise one or two data sources.
#' 
#' Visualise one or two data sources. A histrogram is plotted for each numeric variable and a bar chart for each categorical variable. For each variable that is available in both sources, a grouped histogram/bar chart is plotted in which the distributions within both sources can be compared. Either absolute or relative values can be used.
#'
#' If all values in a categorical variable are unique, then the text "Unique values" is given. If a categorical variable contains more than 30 categories (which may be the case with identification variables), then the text "Too many categories" is given. Note that in the latter case, the values are not unique (for otherwise "Unique values" is given).
#' @param data1 first data source (data.frame)
#' @param data2 second data source (data.frame)
#' @param vars1 names of variables of which grouped histograms are plotted. Should correspond one to one to \code{vars2}
#' @param vars2 names of variables of which grouped histograms are plotted. Should correspond one to one to \code{vars1}
#' @param showAll boolean that specifies whether all variables are plotted, or only the variables specified in \code{vars1} and \code{vars2}.
#' @param colours vector of two colours: one for the first and one for the second data source
#' @param useRelative boolean that specifies whether histogram values (vertical axis) are relative or absolute values.
#' @export
visualise <- function(data1, data2=NULL, vars1=intersect(names(data1), names(data2)), vars2=vars1, showAll=TRUE, colours=c("blue", "green"), useRelative=FALSE) {
	name1 <- deparse(substitute(data1))
	name2 <- deparse(substitute(data2))
	
  
	# check if vars1 exist in data1 and vars2 in data2
	if (!all(vars1 %in% names(data1)))
		stop("Not all variables in vars1 are columns in data1")
	if (!all(vars2 %in% names(data2)))
		stop("Not all variables in vars2 are columns in data2")

  if (is.null(data2)) {
    name <- paste(name1, " (", nrow(data1), " obs.)", sep="")
    showOne <- TRUE
    showTwo <- FALSE
    showBoth <- FALSE
    
    one_names <- names(data1)
  } else {
    name <- paste(name1, " (", nrow(data1), " obs.) and ", 
                  name2, " (", nrow(data2), " obs.)", sep="")

    ## check vars1
    if (length(vars1)==0) stop("Please specify vars1")
    checkVars1 <-(vars1 %in% names(data1))
    if (!all(checkVars1)) 
      stop(paste("Variable(s)", paste(vars1[!checkVars1], collapse=","), 
                 "not valid columnnames in", name1))
    
    ## check vars2
    if (length(vars2)==0) stop("Please specify vars2")
    checkVars2 <-(vars2 %in% names(data2))
    if (!all(checkVars2)) 
      stop(paste("Variable(s)", 
                 paste(vars2[!checkVars2], collapse=","), 
                 "not valid columnnames in", name2))
    
    ## check if lengths are equal
    if (length(vars1)!=length(vars2)) 
      stop("vars1 and vars2 have different lengths.")
    
    
    ## check non-aligned variables
    one_names <- setdiff(names(data1), vars1)
    two_names <- setdiff(names(data2), vars2)
    showOne <- length(one_names) > 0 & showAll
    showTwo <- length(two_names) > 0 & showAll
    showBoth <- TRUE
    
  }
      
  plots <- list()
  if (showBoth) {
	  set12 <- mapply(visualise.pair, 
	                  subset(data1, select=vars1), 
	                  subset(data2, select=vars2),
	                  vars1, vars2,
	                  MoreArgs=list(colour=colours,
	                                useRelative=useRelative),
	                  SIMPLIFY=FALSE)
	  plots <- c(plots, set12)
	}
	
  if (showOne) {
		set1 <- mapply(visualise.single, 
                   subset(data1, select=one_names), 
                   paste(name1, ": ", one_names, sep=""),
                   MoreArgs=list(colour=colours[1],
                                 useRelative=useRelative), SIMPLIFY=FALSE)
    plots <- c(plots, set1)
  }
  if (showTwo) {
    set2 <- mapply(visualise.single, 
                   subset(data2, select=two_names), 
                   paste(name2, ": ", two_names, sep=""),
                   MoreArgs=list(colour=colours[2],
                                 useRelative=useRelative),SIMPLIFY=FALSE)
    plots <- c(plots, set2)
  }
	
	multipleplot(plots, title=
		paste(name, "\n", 
			  ifelse(useRelative, "Relative values", "Absolute values"),
			  sep=""))
	invisible()
}