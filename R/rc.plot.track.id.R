rc.plot.track.id=function(track.id, labels=NULL, degree=0, col='black', custom.track.height=NULL, ...){
	rcPar=rc.get.params()
	chromPar=rc.get.chrom()
	if(is.null(labels)) labels=track.id
	if(is.null(custom.track.height)) custom.track.height=rcPar$track.height
	for(i in 1:length(track.id)){
		pos.xy <- rc.get.trackCoordinates(track.id[i],degree=degree,trackThickness=custom.track.height/2)
		text(pos.xy$x[1], pos.xy$y[1], labels=labels[i], col=col, ...);
	}
	return(invisible())
}
