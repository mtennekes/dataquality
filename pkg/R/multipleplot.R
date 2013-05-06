multipleplot <- function(plots, title="") {
    plot.new()
	grid.newpage()
	par(mar=c(4,4,2,1))
	
	k <- length(plots)
    
    width <- par("din")[1]
    height <- par("din")[2]
    
    mx <- k
    numbers <- matrix(rep(1:mx, mx) * rep(1:mx, each=mx), nrow=mx,ncol=mx) 
    optnum <- rep(0,mx)
    
    for (i in 1:mx) {
        optnum[i] <- min(100,which(numbers[i,]>=k))
    }
    optnum <- unique(optnum)
    optnum <- optnum[optnum!=100]
    optn <- length(optnum)
    minAsp <- 0
    for (i in 1:optn){
        rW <- optnum[i]/width
        cH <- optnum[optn+1-i]/height
        aspR <- min(rW/cH, cH/rW)
        if (aspR > minAsp) {
            minAsp <- aspR
            minAspI <- i
        }
    }
    
    k.cl <- optnum[minAspI]
    k.rw <- optnum[optn+1-minAspI]
    
    
	if (title!="") {
		pushViewport(viewport(layout=
			grid.layout(2, 1, heights=unit(c(ifelse(title=="", 0, 2), 1), 
										  c("lines", "null")))))

		pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
		grid.text(title)
		popViewport()

		pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1))
		
	}
	
	pushViewport(viewport(layout=grid.layout(k.rw, k.cl)))
	
	rw <- 1
	cl <- 1
	for (i in 1:k) {
		if (class(plots[[i]])=="ggplot") {
			theme_set(theme_bw())
			print(plots[[i]], vp = viewport(layout.pos.row = rw, layout.pos.col = cl))
		} else if (class(plots[[i]])=="histogram") {
			pushViewport(viewport(layout.pos.row = rw, layout.pos.col = cl))
			grid.rect(width=0.9, 
					  height=unit(0.99,"npc")-unit(1,"lines"), 
					  y=0.99, just="top")

			ploti <- plots[[i]]
			
            grid.text(ploti$var, y=unit(0.8, "lines"), gp=gpar(cex=0.8))
      

			
			y_scale <- pretty(c(0, ploti$aggr), n=4)
			
			par(new=TRUE, fig=gridFIG(), cex=0.8, mar=c(3,4,1,1))
			bp <- barplot(ploti$aggr, 
						  col=ploti$col,
						  space=ploti$space,
						  axes = FALSE, axisnames = FALSE,
						  xlab=NULL,
						  ylim=c(0, tail(y_scale, 1)))
			
			labels <- ploti$labels
			k <- length(labels)
			if (k >=10) {
				ind <- unique(c(1, floor(k*c(.25,.5, .75)), k))
				labels <- rep(NA, k)
				labels[ind] <- ploti$labels[ind]
			}
			labels <- substr(labels, 1, 10)

			if (ploti$twogroups) {
				labels <- rep(labels, each=2)	
				labels[seq(1, length.out=length(ploti$labels), by=2)] <- NA
			}

			text(bp, par("usr")[3], 
				 labels = labels,
				 srt= 45,
				 adj = 1, 
				 cex = 0.8,
				 xpd = TRUE)
				
			y_scale <- pretty(c(0, ploti$aggr), n=4)
			if (ploti$useRelative) 
				y_labels <- paste(y_scale, "%", sep="")
			else y_labels <- y_scale
			axis(2, cex.axis = 0.8, las=1, labels=y_labels, at=y_scale)

			popViewport()
		} else if (class(plots[[i]])=="textMessage") {
			pushViewport(viewport(layout.pos.row = rw, layout.pos.col = cl))
			grid.rect(width=0.9, 
					  height=unit(0.99,"npc")-unit(1,"lines"), 
					  y=0.99, just="top")
			grid.text(plots[[i]]$text, gp=gpar(cex=0.8))
			grid.text(plots[[i]]$var, y=unit(0.8, "lines"), gp=gpar(cex=0.8))
			popViewport()
		} else if (class(plots[[i]])=="marginPlot"){
			pushViewport(viewport(layout.pos.row = rw, layout.pos.col = cl))

            # create rectangle viewport
            width <- convertWidth(unit(1,"npc"), "inches",valueOnly = TRUE)
			height <- convertHeight(unit(1,"npc"), "inches",valueOnly = TRUE)
			minWH <- min(width, height)
            pushViewport(viewport(width=minWH, height=minWH,default.units="inches"))
            
			par(new=TRUE, fig=gridFIG(), cex=0.8, mar=c(4,4,2,2))
			marginplot(plots[[i]]$data, 
					   xlab=plots[[i]]$xlab, ylab=plots[[i]]$ylab)
 			popViewport(2)
		}
		
		cl <- cl + 1
		if (cl > k.cl) {
			cl <- 1
			rw <- rw + 1
		}
	}
    popViewport(0)
    par(fig=c(0,1,0,1))
}