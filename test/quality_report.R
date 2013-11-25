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

alignment.plot(1000, 401, 400, "sam", "reg_t1")

quality.report(src.data=sam,
			   src.vars=names(sam),
			   src.keys="id",
			   ref.data=reg_t1,
			   ref.vars=names(reg_t1),
			   ref.keys="id_code",
			   check.format=c(idcode="^S[0-9]+$"),
			   output="report.html")

quality.report(src.data=reg_t1,
			   src.vars=names(reg_t1),
			   src.keys="id_code",
			   ref.data=reg_t2,
			   ref.vars=names(reg_t2),
			   ref.keys="id_code",
			   align.vars=list(c("sex", "sex"), c("age", "age")),
			   check.format=c(id_code="^S[0-9]+$"),
			   output="report.html")