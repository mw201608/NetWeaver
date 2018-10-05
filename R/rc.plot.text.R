rc.plot.text=function(Data, track.id, col='black', custom.track.height=NULL, ...){
	rc.check.textData(Data)
	rcPar=rc.get.params()
	if(is.null(custom.track.height)) custom.track.height=rcPar$track.height
	if(length(col) < nrow(Data)) col=rep(col,length.out=nrow(Data))
	for(i in 1:nrow(Data)){
		Chr=Data[i,'Chr']
		Start=Data[i,'Pos']
		pos.xy <- rc.get.trackCoordinates(track.id,Start=Start,End=Start,Chr=Chr,trackThickness=custom.track.height)
		text(pos.xy$x[1], pos.xy$y[1], labels=Data[i,'Label'], col=col[i], ...);
	}
	return(invisible())
}
rc.check.textData=function(Data){
	if(! all(c('Chr','Pos','Label') %in% colnames(Data))) stop('Invalid input data format\n')
	return(invisible())
}
