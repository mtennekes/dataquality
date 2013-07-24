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



data(sam)
data(reg_t1)
quality.report(src.data=sam,
						   src.vars=names(sam),
						   src.keys="id",
						   ref.data=reg_t1,
						   ref.vars=names(reg_t1),
						   ref.keys="id_code",
						   output="report.html")
