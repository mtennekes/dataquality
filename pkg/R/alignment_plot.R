alignment.plot <- function(src.nrow, ref.nrow, matched.nrow, src.name="source", ref.name="reference") {
	require(RColorBrewer)
	require(grid)
	grid.newpage()	
	datnames <- c(src.name, ref.name)
	datwidths <- convertWidth(stringWidth(datnames),unitTo="npc", valueOnly=TRUE)
	
	
	parts <- c(src.nrow - matched.nrow,
			   matched.nrow,
			   ref.nrow - matched.nrow)
	total <- sum(parts)
	
	heights <- parts/total
	ys <- c(cumsum(c(0,heights))[1:3],1)
	numberwidth <- convertWidth(stringWidth("1000000"),unitTo="npc", valueOnly=TRUE)
	
	pushViewport(viewport(layout=grid.layout(2, 6, 
											 widths=unit(c(numberwidth,numberwidth,1,1,numberwidth,numberwidth), c("npc", "npc", "null", "null", "npc", "npc")),
											 heights=unit(c(1.5,1), c("lines", "null")))))
	
	cellplot(1, 3, e={
		grid.text(datnames[1])
	})
	
	cellplot(1, 4, e={
		grid.text(datnames[2])
	})
	
	ys_numbers <- (ys[2:4]+ys[1:3])/2
	
	brewer.set1 <- brewer.pal(9, name="Set1")
	
	cellplot(2, 1, e={
		grid.text(parts[1:2], x=.9, y=ys_numbers[1:2], just="right")
	})
	
	src_perc <- round(parts[1:2]/sum(parts[1:2])*100, 2)
	ref_perc <- round(parts[2:3]/sum(parts[2:3])*100, 2)
	cellplot(2, 2, e={
		grid.text(paste0("(", src_perc, "%)"), x=.9, y=ys_numbers[1:2], just="right")
	})
	
	
	cellplot(2, 5, e={
		grid.text(parts[2:3], x=.9, y=ys_numbers[2:3], just="right")
	})
	cellplot(2, 6, e={
		grid.text(paste0("(", ref_perc, "%)"), x=.9, y=ys_numbers[2:3], just="right")
	})
	
	cellplot(2, 3, e={
		grid.rect(y=ys[1], height=sum(heights[1:2]), gp=gpar(fill=brewer.set1[2], col=NA), just=c("bottom"))	
	})
	
	cellplot(2, 4, e={
		grid.rect(y=ys[2], height=sum(heights[2:3]), gp=gpar(fill=brewer.set1[3], col=NA), just=c("bottom"))	
	})	
	
	cellplot(2, 3:4, e={
		grid.polyline(y=rep(ys[2:3], each=2), x=rep(c(0,1), 2), id=rep(1:2, each=2))
	})
}
