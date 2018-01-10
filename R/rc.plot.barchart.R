rc.plot.barchart=function(Data, track.id, data.col, color.col=NULL, bar.color=NULL, track.color=NA, track.border=NULL, custom.track.height=NULL, ratio=FALSE){
	if(is.null(data.col) || length(data.col)==1) return(rc.plot.histogram(Data=Data, track.id=track.id, data.col=data.col, color.col=color.col, color.gradient=NULL, fixed.height=FALSE, track.color=track.color, track.border=track.border, custom.track.height=custom.track.height, max.value=NULL))
	rc.check.histogramData(Data)
	rc.plot.track(track.id,border=track.border,col=track.color,custom.track.height=custom.track.height)
	rcPar=rc.get.params()
	Rs=rowSums(Data[,data.col,drop=FALSE],na.rm=TRUE)
	max.value=ifelse(ratio,1,max(Rs,na.rm=TRUE))
	if(is.null(custom.track.height)) custom.track.height=rcPar$track.height
	if(is.null(bar.color)) bar.color <- gray.colors(length(data.col))
	if(length(bar.color) < length(data.col)) stop("The length of bar.color is smaller than the length of data.col\n")
	for(i in 1:nrow(Data)){
		Chr=Data[i,'Chr']
		Start=Data[i,'Start']
		End=Data[i,'End']
		d1=unlist(Data[i,data.col])
		jj=which(!is.na(d1))
		d1[is.na(d1)]=0
		d1=cumsum(d1)
		d1=d1/ifelse(ratio,d1[length(d1)],max.value)
		for(j in rev(jj)){
			thick <- custom.track.height*d1[j]
			pos.xy <- rc.get.ringCoordinates(track.id,Start=Start,End=End,Chr=Chr,ringThickness=thick)
			polygon(pos.xy$x, pos.xy$y, col=bar.color[j], border=NA)
		}
	}
	return(invisible())
}
