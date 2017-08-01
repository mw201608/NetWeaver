rc.plot.ribbon=function(Data, track.id, color.col = NULL, twist = FALSE){
	rc.check.ribbonData(Data,color.col)
	rcPar=rc.get.params()
	chromPar=rc.get.chrom()
	baseUnits=rc.get.baseUnits()
	stepSize=baseUnits$stepSize
	#
	data.points=lapply(1:nrow(Data),function(i){
		iChr1=chromPar[[Data[i,'Chr1']]]
		Pos1 = Data[i,'Start1'] - iChr1['Start'] + 1 + iChr1['cumStart']
		Pos2 = Data[i,'End1'] - iChr1['Start'] + 1 + iChr1['cumStart']
		iChr2=chromPar[[Data[i,'Chr2']]]
		Pos3 = Data[i,'Start1'] - iChr2['Start'] + 1 + iChr2['cumStart']
		Pos4 = Data[i,'End2'] - iChr2['Start'] + 1 + iChr2['cumStart']
		c(Pos1,Pos2,Pos3,Pos4)
	})
	data.points=do.call(rbind,data.points)
	if(! is.null(color.col)){
		ribbon.colors <- Data[,color.col]
	}else{
		ribbon.colors=rep(rcPar$color.line,nrow(Data))
	}
	bc.point.num = 1000; tx=seq(0, 1, length.out=bc.point.num); mtx2=(1 - tx)^2; tx2=tx^2;
	if (twist == TRUE) data.points[,3:4]=data.points[,c(4,3)]
	#
	for (i in 1:nrow(Data)) {
		S1 <- data.points[i, 1]
		E1 <- data.points[i, 2]
		S2 <- data.points[i, 3]
		E2 <- data.points[i, 4]
		P0.1 <- do.call(cbind,rc.get.coordinates(track.id,Pos=E1,innerSide=FALSE))
		P2.1 <- do.call(cbind,rc.get.coordinates(track.id,Pos=S2,innerSide=FALSE))
		line1 <- rc.link.line(P0.1, P2.1, mtx2=mtx2, tx2=tx2)
		P0.2 <- do.call(cbind,rc.get.coordinates(track.id,Pos=E2,innerSide=FALSE))
		P2.2 <- do.call(cbind,rc.get.coordinates(track.id,Pos=S1,innerSide=FALSE))
		line2 <- rc.link.line(P0.2, P2.2, mtx2=mtx2, tx2=tx2)
		cut1 <- rc.get.coordinates(track.id,Pos=seq(S1,E1,by=stepSize),innerSide=FALSE)
		cut2 <- rc.get.coordinates(track.id,Pos=seq(S2,E2,by=ifelse(twist,- stepSize, stepSize)),innerSide=FALSE)
		pos.x <- c(cut1$x, line1$pos.x, cut2$x, line2$pos.x)
		pos.y <- c(cut1$y, line1$pos.y, cut2$y, line2$pos.y)
		polygon(pos.x, pos.y, border = NA, col = ribbon.colors[i])
	}
}
rc.check.ribbonData=function(Data,color.col){
	Data=as.data.frame(Data)
	if(! all(c('Chr1','Start1','End1','Chr2','Start2','End2') %in% colnames(Data))) stop('Data is not in proper format\n')
	if(any(Data$Start1 > Data$End1 | Data$Start2 > Data$End2)) stop('Start must be always no larger than End\n')
}
