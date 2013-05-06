#' Describes the relationship between two variables in a data source.
#' 
#' Describes the relationship between two variables in a data source. Both 1-to-n and m-to-1 relations are explored.
#'
#' @param data data source (data.frame)
#' @param var1 name of the first variable of \code{data}
#' @param var2 name of the second variable of \code{data}
#' @param draw logical that determines whether to plot a graph
#' @param graph.lowerbound.numberV1perV2 lowerbound for the number of units \code{var1} per unit \code{var2} that are plot in a graph
#' @param graph.lowerbound.numberV2perV1 lowerbound for the number of units \code{var2} per unit \code{var1} that are plot in a graph
#' @return the descriptions are silently returned in a list
#' @export
relation <- function(data, var1, var2, draw=FALSE, graph.lowerbound.numberV1perV2=NULL, graph.lowerbound.numberV2perV1=NULL) {
    
    V1 <- NULL; rm(V1); #trick R CMD check
    V2 <- NULL; rm(V2); #trick R CMD check
    
    data <- data.table(V1=data[[var1]], V2=data[[var2]])
    
    agg1 <- data[, list(count=length(V2)), by=V1]
    agg2 <- data[, list(count=length(V1)), by=V2]
    
    t1 <- table(agg1$count)
    t2 <- table(agg2$count)
    
    cat(paste("Number of units ", var1, ": ", sum(t1), "\n", sep=""))
    cat(paste("Number of units ", var2, ": ", sum(t2), "\n", sep=""))
    cat("------------------------------------\n")
    print(d1 <- describe(agg2$count, 
             descript=paste("Number of units ", var1, " per unit ", var2, ":\n", sep="")))
    cat("------------------------------------\n")
    print(d2 <- describe(agg1$count, 
             descript=paste("Number of units ", var2, " per unit ", var1, ":\n", sep="")))
    
    if (draw) {
    	
    	setkey(data, V1)
    	data <- merge(data, agg1)
    	setkey(data, V2)
    	data <- merge(data, agg2)

    	if (!missing(graph.lowerbound.numberV2perV1)) {
    		data <- data[data$count.x>=graph.lowerbound.numberV2perV1,]
    	}
    	
    	if (!missing(graph.lowerbound.numberV1perV2)) {
    		data <- data[data$count.y>=graph.lowerbound.numberV1perV2,]
    	}
    	
        g<-graph.edgelist(as.matrix(data[,list(V1,V2)]))
        V(g)$label <- V(g)$name
        
    	suppressWarnings({
    		plot(g, layout=layout.graphopt(g),vertex.size=8, edge.arrow.width=0.5)
    	})
    }
    
    invisible(list(nV1perV2=d1, nV2perV1=d2))
}
