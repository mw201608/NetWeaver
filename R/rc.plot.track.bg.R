rc.plot.track.bg=function(track.id, bg.color='grey', custom.track.height=NULL){
	if(is.null(bg.color) || bg.color == 'white') return(invisible())
	rcPar=rc.get.params()
	chromPar=rc.get.chrom()
	if(is.null(custom.track.height)) custom.track.height=rcPar$track.height
	for(i in 1:length(chromPar)){
		Chr=names(chromPar)[i]
		iChr=chromPar[[i]]
		Start=iChr['Start']
		End=iChr['End']
		pos.xy <- rc.get.trackCoordinates(track.id,Start=Start,End=End,Chr=Chr)
		polygon(pos.xy$x, pos.xy$y, col=bg.color);
	}
	return(invisible())
}
