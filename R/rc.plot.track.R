rc.plot.track=function(track.id, border='black', col=NA, custom.track.height=NULL){
	if(is.null(border)) border='black'
	if(is.na(border) && is.na(col)) return(invisible())
	rcPar=rc.get.params()
	chromPar=rc.get.chrom()
	if(is.null(custom.track.height)) custom.track.height=rcPar$track.height
	for(i in 1:length(chromPar)){
		Chr=names(chromPar)[i]
		iChr=chromPar[[i]]
		Start=iChr['Start']
		End=iChr['End']
		pos.xy <- rc.get.ringCoordinates(track.id,Start=Start,End=End,Chr=Chr)
		polygon(pos.xy$x, pos.xy$y, border=border, col=col);
	}
	return(invisible())
}
