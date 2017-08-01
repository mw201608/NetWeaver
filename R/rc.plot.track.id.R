rc.plot.track.id=function(track.id, labels=NULL, degree=0, col='black', custom.track.height=NULL, ...){
	rcPar=rc.get.params()
	chromPar=rc.get.chrom()
	if(is.null(labels)) labels=track.id
	if(is.null(custom.track.height)) custom.track.height=rcPar$track.height
	for(i in 1:length(track.id)){
		Chr=names(chromPar)[i]
		iChr=chromPar[[i]]
		Start=iChr['Start']
		End=iChr['End']
		pos.xy <- rc.get.ringCoordinates(track.id[i],degree=degree,ringThickness=custom.track.height/2)
		text(pos.xy$x[1], pos.xy$y[1], labels=labels[i], col=col, ...);
	}
	return(invisible())
}
