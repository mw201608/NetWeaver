rc.plot.heatmap=function(Data, track.id, color.gradient=NULL, track.color=NA, track.border=NULL){
	Data=as.matrix(Data)
	rc.check.heatmapData(Data)
	chromPar=rc.get.chrom()
	if(!is.null(color.gradient)){
		mx=max(Data,na.rm=TRUE)
		cols=apply(Data,2,function(x,color.gradient,mx) color.gradient[pmax(1,floor(x*length(color.gradient)/mx))],color.gradient=color.gradient,mx=mx)
	}
	HistData=data.frame(Chr=colnames(Data), Start=1, End=100, Data=NA, Col=NA, stringsAsFactors=FALSE)
	for(i in 1:nrow(HistData)){
		HistData$Start[i]=chromPar[[HistData$Chr[i]]]['Start']
		HistData$End[i]=chromPar[[HistData$Chr[i]]]['End']
	}
	data.col <- 4
	color.col <- 5
	for(i in 1:nrow(Data)){
		if(!is.null(color.gradient)){
			HistData[,color.col]=cols[i,]
		}else{
			HistData[,color.col]=Data[i,]
		}
		rc.plot.histogram(HistData, track.id+i-1, color.col=color.col, fixed.height=TRUE,
			track.color=track.color, track.border=track.border)
	}
}
rc.check.heatmapData=function(Data){
	if(is.null(colnames(Data))) stop('Data must have column names\n')
	chromPar=rc.get.chrom()
	if(! all(colnames(Data) %in% names(chromPar))) stop('Unrecognized column names found. Please make sure all elements in the column names have been specified with cyto information during plot initialization.\n')
}
