visualise.single <- function(v, name, colour, useRelative) {
	if (anyDuplicated(v)==0) {
		plots <- list(text="Unique values", var=name)
		class(plots) <- "textMessage"
	} else {
		if (class(v)[1] %in% c("integer", "numeric")) {
			rangeDat <- range(v, na.rm=TRUE)
			scale <- pretty(rangeDat, n=30)
			v <- cut(v, breaks=scale, 
					 include.lowest=TRUE, right=FALSE)
		    isnumeric <- TRUE	
		} else isnumeric <- FALSE
		v <- withNA(v, useNA="ifany")

		lev <- levels(v)
		nlev <- nlevels(v)
	
		if (nlev > 30 && !isnumeric) {
			plots <- list(text="Too many categories",
						   var=name)
			class(plots) <- "textMessage"
		} else {
			aggr <- table(v)
			if (useRelative) aggr <- aggr / sum(aggr) * 100

			col <- rep(colour, nlev)
			col[lev=="missing"] <- "#FF1414"
			space <- c(0,1)
			plots <- list(aggr=aggr, 
							   col=col, 
							   space=space,
							   labels=lev,
							   var=name,
							   twogroups=FALSE,
						  useRelative=useRelative)
			class(plots) <- "histogram"
		}
	}
	return(plots)
}