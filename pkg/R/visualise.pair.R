visualise.pair <- function(v1, v2, name1, name2, colours, useRelative) {
	name <- ifelse(name1==name2, name1, paste(name1, name2))
	class1 <- class(v1)[1]
	class1 <- ifelse(class1=="integer", "numeric", class1)
	class2 <- class(v2)[1]
	class2 <- ifelse(class2=="integer", "numeric", class2)
	
	
	if ((anyDuplicated(v1)==0) && (anyDuplicated(v2)==0)) {
		plots <- list(text="Unique values", var=name)
		class(plots) <- "textMessage"
	} else if (class1!=class2){
		plots <- list(text="Error: different classes", var=name)
		class(plots) <- "textMessage"
	} else {
		if (class1=="numeric") {
			rangeDat <- c(range(v1, na.rm=TRUE), range(v2, na.rm=TRUE))
			scale <- pretty(rangeDat, n=30)
			
			v1 <- cut(v1, 
							  breaks=scale, 
							  include.lowest=TRUE, right=FALSE)
			v2 <- cut(v2, 
							  breaks=scale, 
							  include.lowest=TRUE, right=FALSE)
		}

		## check consistency levels
		if (!identical(levels(v1), levels(v2))) {
			if (!setequal(levels(v1), levels(v2))) {
				plots <- list(text="Error: levels not consistent", 
							  var=name)
				class(plots) <- "textMessage"
			} else {
				v2 <- factor(v2, levels=levels(v1))
			}
		} else if (nlevels(v1) > 30) {
			plots <- list(text="Too many categories", 
						  var=name)
			class(plots) <- "textMessage"
		} else {	
			if (any(is.na(v1)) || any(is.na(v2))) {
				v1 <- withNA(v1, useNA="always")
				v2 <- withNA(v2, useNA="always")
			}
			lev <- levels(v1)
			nlev <- nlevels(v1)
	
			aggr1 <- table(v1)
			aggr2 <- table(v2)
			
			if (useRelative) {
				aggr1 <- aggr1 / sum(aggr1) * 100
				aggr2 <- aggr2 / sum(aggr2) * 100
			}
			
			
			aggr <- numeric(nlev*2)
			aggr[seq(1, length.out=nlev, by=2)] <- aggr1
			aggr[seq(2, length.out=nlev, by=2)] <- aggr2
			
			col1 <- rep(colours[1], nlev)
			col1[lev=="missing"] <- "#FF1414"
			col2 <- rep(colours[2], nlev)
			col2[lev=="missing"] <- "#FF1414"

			col <- character(nlev*2)
			col[seq(1, length.out=nlev, by=2)] <- col1
			col[seq(2, length.out=nlev, by=2)] <- col2
			space <- rep(c(1, 0), times=nlev)
			
			plots <- list(aggr=aggr, 
							   col=col, 
							   space=space,
							   labels=lev,
							   var=name,
							   twogroups=TRUE,
						  useRelative=useRelative)
			class(plots) <- "histogram"
		}
	}

	return(plots)
}
