#' Quality report
#'
#' Quality report
#'
#' @param data data
#' @param output name of genereted html document
#' @import knitr
#' @export
quality.report <- function(data,
						   keys.data,
						   check.pattern,
						   data2,
						   keys.data2,
						   output="report.html") {
	name.data <- deparse(substitute(data))
	

	data <- sam
	name.data<-"sam"
	output <- "sam_report.html"
	Rquire(knitr)
	
	
	knit2html("./inst/report/report.Rmd", output=output)
	visualise(data)
}