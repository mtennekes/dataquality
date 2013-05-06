#' Visually compare two data sources
#'
#' Visually compare two data sources. The data sources are joined by one or more keys. Only key values that occur in both sources are taken into account (where duplicated are omitted). For each numerical variable, a scatter plot with box plots is plotted, and for each categorical variable a fluctuation plot. Missing values are taken into account.
#'
#' @param data1 first data source (data.frame)
#' @param data2 second data source (data.frame)
#' @param key1 name(s) of key variable(s) from \code{data1}. Should correspond one to one to \code{key2}. 
#' @param key2 name(s) of key variable(s) from \code{data2}. Should correspond one to one to \code{key1}.
#' @param vars1 name(s) of the variable(s) from \code{data1} that are visually compared (one to one) to \code{vars2}.
#' @param vars2 name(s) of the variable(s) from \code{data2} that are visually compared (one to one) to \code{vars1}.
#' @export
compare <- function(data1, data2, key1 = intersect(names(data1), names(data2))[1], key2 = key1, vars1 = setdiff(intersect(names(data1), names(data2)), key1), vars2 = vars1) {
	
	name1 <- deparse(substitute(data1))
	name2 <- deparse(substitute(data2))
	
	if (is.na(key1)) 
		stop("No common key found. Please specify key1 and key2.")
	checkKey1 <-(key1 %in% names(data1))
	if (!all(checkKey1)) 
		stop(paste("Key(s)", 
				   paste(key1[!checkKey1], collapse=","), 
				   "not valid columnnames in", name1))
	
	checkKey2 <-(key2 %in% names(data2))
	if (!all(checkKey2)) 
		stop(paste("Key(s)", 
				   paste(key2[!checkKey2], collapse=","), 
				   "not valid columnnames in", name2))
	
	if (length(key1) != length(key2)) 
		stop("Please specify the same number 
			 of keys in keys1 and keys2.")
	
	
	if (length(vars1)==0) stop("Please specify vars1")
	checkVars1 <-(vars1 %in% names(data1))
	if (!all(checkVars1)) 
		stop(paste("Variable(s)", 
				   paste(vars1[!checkVars1], collapse=","), 
				   "not valid columnnames in", name1))
	
	if (length(vars2)==0) stop("Please specify vars2")
	checkVars2 <-(vars2 %in% names(data2))
	if (!all(checkVars2)) 
		stop(paste("Variable(s)",
				   paste(vars2[!checkVars2], collapse=","), 
				   "not valid columnnames in", name2))

	if (length(vars1) != length(vars2)) 
		stop("Please specify the same number 
			 of variables in vars1 and vars2.")


	if (!class(data1)[1]=="data.table") {
		data1 <- data.table(data1)
	}

	if (!class(data2)[1]=="data.table") {
		data2 <- data.table(data2)
	}

	setkeyv(data1, key1)
	setkeyv(data2, key2)

	nrowData1 <- nrow(data1)
	.SD <- NULL
	data1 <- data1[, .SD[1,], by=key1]
	if ((diff1 <- (nrowData1 - nrow(data1)))!=0) 
		warning(paste(diff1, "rows in", name1, 
					  "have duplicated keys, and are therefore omitted"))

	nrowData2 <- nrow(data2)	
	data2 <- data2[, .SD[1,], by=key2]
	if ((diff2 <- (nrowData2 - nrow(data2)))!=0) 
		warning(paste(diff2, "rows in", name2, 
					  "have duplicated keys, and are therefore omitted"))

    
	data <- data1[data2, nomatch=0]

    k <- length(vars1)
	plots <- list(k)
	for (i in 1:k) {
		var1 <- paste(name1, vars1[i], sep=": ")
		var2 <- paste(name2, vars2[i], sep=": ")
		if (class(data[[vars1[i]]]) %in% c("integer", "numeric")) {
			plots[[i]] <- list(data=data[,c(
				match(vars1[i], names(data1)), 
				match(vars2[i],
					  names(data2))+
					  	ncol(data1)-length(key1)),with=FALSE],
							   xlab=var1,
							   ylab=var2)
			class(plots[[i]]) <- "marginPlot"
		} else {
	#browser()	
			plots[[i]] <- ggfluctuation(table(
                data[[match(vars1[i], names(data1))]],
                data[[match(vars2[i], 
                            names(data2))+ ncol(data1)-length(key1)]],
                useNA="always")) + 
				xlab(var1) + ylab(var2) + 
				opts(legend.position="none")
		}
	}
	
	multipleplot(plots, title=paste(name1, " compared to ", 
									name2, " (joined by ", 
									key1, ")\n", nrow(data), 
									" common observations", sep=""))
	invisible()
}