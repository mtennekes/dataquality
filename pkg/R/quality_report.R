#' Quality report
#'
#' Quality report
#'
#' @param data data
#' @param output name of genereted html document
#' @import knitr
#' @import data.table
#' @export
quality.report <- function(src.data,
						   src.vars,
						   src.keys,
						   ref.data,
						   ref.vars,
						   ref.keys,
						   align.vars=NULL,
						   check.format=NULL,
						   output="report.html") {
	src.name <- deparse(substitute(src.data))
	ref.name <- deparse(substitute(ref.data))
	
	src.vars <- setdiff(src.vars, src.keys)
	ref.vars <- setdiff(ref.vars, ref.keys)
	
	
	src.data <- convert.data.table(src.data, src.keys, src.vars)
	ref.data <- convert.data.table(ref.data, ref.keys, ref.vars)
	
	src.align.vars <- sapply(align.vars, function(x)paste0(x[1], ".src"))
	ref.align.vars <- sapply(align.vars, function(x)paste0(x[2], ".ref"))
	
	src.vars.new <- paste0(src.vars, ".src")
	ref.vars.new <- paste0(ref.vars, ".ref")
	setnames(src.data, src.vars, src.vars.new)
	setnames(ref.data, ref.vars, ref.vars.new)
	
	print(nrow(src.data))
	src.list <- remove.duplicates(src.data)
	ref.list <- remove.duplicates(ref.data)
	
	src.data <- copy(src.list$data)
	src.duplicates <- src.list$duplicates
	print(nrow(src.data))
	
	ref.data <- ref.list$data
	ref.duplicates <- ref.list$duplicates
	matched.data <- src.data[ref.data, nomatch=0]
	src.keys2 <- src.keys
	
	require(knitr)
	browser()
	knit2html("./inst/report/report.Rmd", output=output)
}



#' Convert data table
#'
#' Convert data table
#'
#' @param data data
#' @param keys keys
#' @param vars vars
#' @import data.table
convert.data.table <- function(data, keys, vars) {
	if (is.ffdf(data)) {
		stop("ffdf not supported yet")
	} else if (!is.data.table(data)) {
		if (!is.data.frame(data)) stop("data is not a data.frame")
		data <- as.data.table(data)
	}
	
	data.subset <- subset(data, select=c(keys, vars))
	setkeyv(data.subset, keys)
	data.subset
}	

#' Remove duplicates
#'
#' Remove duplicates
#'
#' @param data data
#' @import data.table
remove.duplicates <- function(data) {
	duplicates <- duplicated(data)
	duplicates <- data[duplicates, ]
	data.unique <- unique(data)
	
	setkeyv(data.unique, key(data))
	setkeyv(duplicates, key(data))
	
	list(duplicates=duplicates, data=data.unique)
}

cellplot <- function(x,y, vp=NULL, e){
	name <- paste("(", deparse(substitute(x)),",",deparse(substitute(y)), ")", sep="")
	pushViewport(viewport( name=name, layout.pos.row=x, layout.pos.col=y))
	n <- 1
	if (!is.null(vp)){ 
		pushViewport(vp)
		n <- n + 1
	}
	e
	popViewport(n=n)
}