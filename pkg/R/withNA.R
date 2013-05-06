withNA <- function(v, useNA="ifany") {
	if (useNA=="always" || (useNA=="ifany" && any(is.na(v)))) {
		v <- factor(v, levels=c("missing", levels(v)))
		v[is.na(v)] <- "missing"
	}
	return(v)
}