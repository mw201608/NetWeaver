rc.plot.sunburst2=function(Data, root=NULL){
	stopifnot(is.data.frame(Data))
	library('ggplot2')
	colnames(Data)[1:2] <- c('child', 'parent')
	if(is.null(root)){
		root <- Data$parent[!Data$parent %in% Data$child[Data$parent != Data$child]]
		root <- unique(root)
	}else{
		all1 <- unique(root)
		while(TRUE){
			all2 <- union(all1, Data$child[Data$parent %in% all1])
			if(length(all1)==length(all2)) break
			all1 <- all2
		}
		Data <- Data[Data$parent %in% all1, ]
	}
	#
    df1 <- df0(x = 0.5, y = 1 : length(root) - 0.5, val = root, w = 1, h = 1)
	cur <- root
	while(TRUE){
		new <- c()
		for(c1 in cur){
			i <- which(Data[, 'parent'] == c1)
			n <- length(i)
			if(n == 0) next
			i <- rev(i)
			new <- union(new, Data[i, 'child'])
			d1 <- df1[df1[, 'val'] == c1, ]
			h1 <- d1[, 'h'] / n
			y1 <- c(d1[, 'y'] - d1[, 'h']/2) + h1 * c(1:n) - h1 / 2
			df1 <- rbind(df1, df0(x = d1[, 'x'] + 1, y = y1, val = Data[i, 'child'], w = 1, h = h1))
		}
		if(length(new) == 0 ) break
		cur <- new
	}
	df1$angle <- sapply(df1$y / length(root), compute_angle)
	p1 <- ggplot(data = df1, aes(x = x, y = y, width = w, height = h)) +
		geom_tile(fill = "white", color = "black") + 
		geom_text(aes(label = val, angle = angle), colour = "black") +
		coord_polar('y')
	p1 + theme_classic() + theme(axis.title = element_blank(), axis.line = element_blank(), 
		axis.text = element_blank(), axis.ticks = element_blank())
}
compute_angle <- function(x){
	angle <- -1
	if(x < 0.25) { # 1st q [90,0]
		angle <- 90 - (x / 0.25) * 90
	} else if(x < 0.5) { # 2nd q [0, -90]
		angle <- (x - 0.25) / 0.25 * -90
	} else if(x < 0.75) { # 3rd q [90, 0]
		angle <- 90 - ((x - 0.5) / 0.25 * 90)
	} else if(x < 1.00) { # last q [0, -90]
		angle <- ((x - 0.75) / 0.25) * -90
	}
	# Or even more compact, but less readable
	if(x < 0.5) { # 1st half [90, -90]
		angle <- (180 - (x / 0.5) * 180) - 90
	} else { # 2nd half [90, -90]
		angle <- (90 - ((x - 0.5) / 0.5) * 180)
	}
	return(angle)
}

