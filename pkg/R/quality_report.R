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
						   check.pattern,
						   output="report.html") {
	src.name <- deparse(substitute(src.data))
	
	data(sam)
	data(reg_t1)
	
	src.data <- sam
	src.name <-"sam"
	src.keys <- "id"
	src.vars <- names(sam)
	
	ref.data <- reg_t1
	ref.name <-"reg_t1"
	ref.keys <- "id_code"
	ref.vars <- names(reg_t1)


	src.vars <- setdiff(src.vars, src.keys)
	ref.vars <- setdiff(ref.vars, ref.keys)
	
	
	src.data <- convert.data.table(src.data, src.keys, src.vars)
	ref.data <- convert.data.table(ref.data, ref.keys, ref.vars)
	
	setnames(src.data, src.vars, paste0(src.vars, ".src"))
	setnames(ref.data, ref.vars, paste0(ref.vars, ".ref"))
	
	src.list <- remove.duplicates(src.data)
	ref.list <- remove.duplicates(ref.data)
	
	src.data <- src.list$data
	src.duplicates <- src.list$duplicates

	ref.data <- ref.list$data
	ref.duplicates <- ref.list$duplicates
	
	merged.data <- src.data[ref.data,nomatch=0]
	
	## plot
	grid.newpage()	
	datnames <- c(src.name, ref.name)
	datwidths <- convertWidth(stringWidth(datnames),unitTo="npc", valueOnly=TRUE)
	
	
	pushViewport(viewport(grid.layout(2, 2, widths=
									  	unit(c(max(datwidths)+.1, 1), c("npc", "null")))))
	
	parts <- c(nrow(src.data) - nrow(merged.data),
			   nrow(merged.data),
			   nrow(ref.data) - nrow(merged.data))
	total <- sum(parts)
	
	widths <- parts/total
	
	numbers <- as.character(parts)
	convertWidth(stringWidth(numbers),unitTo="npc", valueOnly=TRUE)
	
	
	xs <- c(cumsum(c(0,widths))[1:3],1)
	
	library(RColorBrewer)
	display.brewer.all()
	brewer.set1 <- brewer.pal(9, name="Set1")
	
	grid.rect(x=c(xs[1], xs[2]), y=c(1, .6), width=c(sum(widths[1:2]), sum(widths[2:3])), height=.3, gp=gpar(fill=brewer.set1[2:3], col=NA), just=c("left", "top"))	

	grid.polyline(x=rep(xs, each=2), y=rep(c(0,1), 4), id=rep(1:4, each=2),gp=gpar(lty="dashed"))
	
	
	data <- sam
	name.data<-"sam"
	output <- "sam_report.html"
	require(knitr)
	
	
	knit2html("./inst/report/report.Rmd", output=output)
	visualise(data)
}

convert.data.table <- function(data, keys, vars) {
	if (is.ffdf(data)) {
		stop("ffdf not supported yet")
	} else if (!is.data.table(data)) {
		if (!is.data.frame(data)) stop("data is not a data.frame")
		data <- as.data.table(data)
	}
	
	setkeyv(data, keys)
	subset(data, select=c(keys, vars))
}	

remove.duplicates <- function(data) {
	duplicates <- duplicated(data)
	duplicates <- data[duplicates, ]
	
	list(duplicates=duplicates, data=unique(data))
}