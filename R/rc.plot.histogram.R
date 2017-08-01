rc.plot.histogram=function(Data, track.id, data.col=NULL, color.col=NULL, color.gradient=NULL, fixed.height=FALSE, track.color=NA, track.border=NULL, custom.track.height=NULL, max.value=NULL){
	rc.check.histogramData(Data)
	rc.plot.track(track.id,border=track.border,col=track.color)
	rcPar=rc.get.params()
	if(is.null(data.col)) fixed.height=TRUE
	if(is.null(max.value)) max.value=ifelse(fixed.height==TRUE && is.null(color.gradient),1,max(Data[,data.col],na.rm=TRUE))
	if(is.null(custom.track.height)) custom.track.height=rcPar$track.height
	for(i in 1:nrow(Data)){
		Chr=Data[i,'Chr']
		Start=Data[i,'Start']
		End=Data[i,'End']
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
		pos.xy <- rc.get.ringCoordinates(track.id,Start=Start,End=End,Chr=Chr,ringThickness=thick)
		polygon(pos.xy$x, pos.xy$y, col=Col, border=NA);
	}
	return(invisible())
}
rc.check.histogramData=function(Data){
	Data=as.data.frame(Data)
	if(! all(c('Chr','Start','End') %in% colnames(Data))) stop('Data is not in proper format\n')
	if(any(Data$Start > Data$End)) stop('Start must be always no larger than End\n')
	chromPar=rc.get.chrom()
	if(! all(Data$Chr %in% names(chromPar))) stop('Unrecognized items found in column Chr. Please make sure all elements in Chr have been specified with cyto information during plot initialization.\n')
	for(i in 1:nrow(Data)){
		Chr=Data[i,'Chr']
		iChr=chromPar[[Chr]]
		Start=Data[i,'Start']
		End=Data[i,'End']
		if(Start<iChr['Start'] || End > iChr['End']) stop(paste('Invalid Start/End for row',i))
	}
}
