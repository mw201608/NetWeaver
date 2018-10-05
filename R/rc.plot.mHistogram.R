#plot histogram that across multiple chromosomes
rc.plot.mHistogram=function(Data, track.id, data.col=NULL, color.col=NULL, color.gradient=NULL, fixed.height=FALSE, track.color=NA, track.border=NULL, polygon.border=NULL, custom.track.height=NULL, max.value=NULL){
	rc.check.mHistogramData(Data,data.col,color.col)
	rc.plot.track(track.id,border=track.border,col=track.color,custom.track.height=custom.track.height)
	rcPar=rc.get.params()
	chromPar=rc.get.chrom()
	if(is.null(data.col)) fixed.height=TRUE
	if(is.null(max.value)) max.value=ifelse(fixed.height==TRUE && is.null(color.gradient),1,max(Data[,data.col],na.rm=TRUE))
	if(is.null(custom.track.height)) custom.track.height=rcPar$track.height
	for(i in 1:nrow(Data)){
		Chr1=Data[i,'Chr1']
		iChr1=chromPar[[Chr1]]
		Chr2=Data[i,'Chr2']
		iChr2=chromPar[[Chr2]]
		cumStart = Data[i,'Start1'] - iChr1['Start'] + 1 + iChr1['cumStart']
		cumEnd = Data[i,'End2'] - iChr2['Start'] + 1 + iChr2['cumStart']
		Col=NA
		if(! is.null(color.gradient)){
			cid=floor(Data[i,data.col]*length(color.gradient)/max.value)
			Col=color.gradient[max(cid,1)]
		}else if(! is.null(color.col)){
			Col=Data[i,color.col]
		}else{
			Col=rcPar$color.hist
		}
		if((! is.na(Col)) && Col=='white') Col=NA
		thick=ifelse(fixed.height,custom.track.height,custom.track.height*Data[i,data.col]/max.value)
		pos.xy <- rc.get.trackCoordinates(track.id,Start=cumStart,End=cumEnd,trackThickness=thick)
		polygon(pos.xy$x, pos.xy$y, col=Col, border=polygon.border);
	}
	return(invisible())
}
rc.check.mHistogramData=function(Data,data.col,color.col){
	Data=as.data.frame(Data)
	if(! all(c('Chr1','Start1','Chr2','End2') %in% colnames(Data))) stop('Data is not in proper format\n')
	chromPar=rc.get.chrom()
	if(! all(c(Data$Chr1,Data$Chr2) %in% names(chromPar))) stop('Unrecognized items found in column Chr1/Chr2. Please make sure all elements in Chr1/Chr2 have been specified with cyto information during plot initialization.\n')
	for(i in 1:nrow(Data)){
		Chr1=Data[i,'Chr1']
		Chr2=Data[i,'Chr2']
		iChr1=chromPar[[Chr1]]
		iChr2=chromPar[[Chr2]]
		Start=Data[i,'Start1']
		End=Data[i,'End2']
		if(Start<iChr1['Start'] || End > iChr2['End']) stop(paste('Invalid Start1/End2 for row',i))
	}
}
