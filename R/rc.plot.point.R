rc.plot.point=function(Data, track.id, color.col=NULL, custom.track.height=NULL, ...){
	rc.check.pointData(Data)
	rcPar=rc.get.params()
	if(is.null(custom.track.height)) custom.track.height=rcPar$track.height
	if(is.null(color.col)){
		cols=rep('black',length.out=nrow(Data))
	}else{
		cols=Data[,color.col]
	}
	Data$Height=Data$Height/max(Data$Height,na.rm=TRUE)
	for(i in 1:nrow(Data)){
		Chr=Data[i,'Chr']
		Start=Data[i,'Pos']
		pos.xy <- rc.get.trackCoordinates(track.id,Start=Start,End=Start,Chr=Chr,trackThickness=custom.track.height*Data[i,'Height'])
		points(pos.xy$x[1], pos.xy$y[1], col=cols[i], ...);
	}
	return(invisible())
}
rc.check.pointData=function(Data){
	if(! all(c('Chr','Pos', 'Height') %in% colnames(Data))) stop('Invalid input data format\n')
	return(invisible())
}
