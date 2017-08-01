rc.plot.line=function(Data, track.id, color.col=NULL, custom.length=NULL, ...){
	rc.check.lineData(Data)
	rcPar=rc.get.params()
	if(is.null(custom.length)) custom.length=rcPar$track.height
	for(i in 1:nrow(Data)){
		Chr=Data[i,'Chr']
		Pos=Data[i,'Pos']
		Col=NA
		if(! is.null(color.col)){
			Col=Data[i,color.col]
		}else{
			Col=rcPar$color.line
		}
		if((! is.na(Col)) && Col=='white') Col=NA
		pos.xy <- rc.get.ringCoordinates(track.id,Start=Pos,End=Pos,Chr=Chr,ringThickness=custom.length)
		points(pos.xy$x, pos.xy$y, col=Col, type='l', ...);
	}
	return(invisible())
}
rc.check.lineData=function(Data){
	Data=as.data.frame(Data)
	if(! all(c('Chr','Pos') %in% colnames(Data))) stop('Data is not in proper format\n')
	chromPar=rc.get.chrom()
	if(! all(Data$Chr %in% names(chromPar))) stop('Unrecognized items found in column Chr. Please make sure all elements in Chr have been specified with cyto information during plot initialization.\n')
	for(i in 1:nrow(Data)){
		Chr=Data[i,'Chr']
		iChr=chromPar[[Chr]]
		Pos=Data[i,'Pos']
		if(Pos<iChr['Start'] || Pos > iChr['End']) stop(paste('Invalid Pos for row',i))
	}
}
