changed_values <- function(matched.data, src.align.vars, ref.align.vars) {
	
	k <- length(src.align.vars)
	s <- rep.int(0, k)
	
	for (i in 1:k) s[i] <- length(which(matched.data[[src.align.vars[i]]] ==matched.data[[ref.align.vars[i]]]))
	
	n <- nrow(matched.data)
	
	dat <- data.frame(unchanged=s, unchanged.frac=round(s/n, 4), changed=(n-s), changed.frac=round((n-s)/n, 4))
	row.names(dat) <- substr(src.align.vars, 1, nchar(src.align.vars)-4)
	dat
}