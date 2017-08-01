rc.plot.ideogram=function(track.ids, plot.band=TRUE, plot.chromosome.id=TRUE, chrom.alias=NULL, color.chromosome.id=NULL, cex.text=1, track.border=NULL, las=0, custom.track.height=NULL){
	rcPar=rc.get.params()
	chromPar=rc.get.chrom()
	cyto=rcEnvirInternal[["cyto.info"]]
	cyto['Data']=1 #set arbitrary numeric value for histogram
	if(length(track.ids)==1) track.ids=c(track.ids,track.ids)
	if(plot.band) rc.plot.histogram(cyto, track.ids[2], data.col='Data', color.col='BandColor', fixed.height=TRUE, track.border=track.border,custom.track.height=custom.track.height)
	if(!plot.chromosome.id) return(invisible())
	#
	if(is.null(color.chromosome.id)) color.chromosome.id=sapply(names(chromPar),function(x) 1)
	for(i in 1:length(chromPar)){
		Chr=names(chromPar)[i]
		if(! is.null(chrom.alias)) Chr=chrom.alias[Chr]
		iChr=chromPar[[i]]
		Start = iChr['cumStart']
		mid = floor((iChr['End'] - iChr['Start'] + 1)/2 + iChr['cumStart'])
		pos.xy <- rc.get.coordinates(track.ids[1],Pos=mid,innerSide=FALSE)
		pos=c(mean(pos.xy$x),mean(pos.xy$y))
		if (las==1){
			srt=0
		}else if (las==2){
			srt=rc.compute.degree(mid) * 180/pi
		}else if (las==3){
			srt=90
		}else{ #default
			srt = rc.compute.degree(mid) * 180/pi - 90
		}
		text(pos[1], pos[2], label = Chr,col=color.chromosome.id[Chr],
			srt = srt, cex=cex.text)
	}
	return(invisible())
}
