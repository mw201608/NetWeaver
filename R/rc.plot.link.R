rc.plot.link=function(Data, track.id, data.col=NULL, color.col = NULL, max.lwd=1, sort.links=TRUE){
	rc.check.linkData(Data,data.col,color.col)
	rcPar=rc.get.params()
	chromPar=rc.get.chrom()
	#
	if(sort.links) Data=rc.link.sort(Data)
	#
	data.points=lapply(1:nrow(Data),function(i){
		iChr1=chromPar[[Data[i,'Chr1']]]
		Pos1 = Data[i,'Pos1'] - iChr1['Start'] + 1 + iChr1['cumStart']
		iChr2=chromPar[[Data[i,'Chr2']]]
		Pos2 = Data[i,'Pos2'] - iChr2['Start'] + 1 + iChr2['cumStart']
		c(Pos1,Pos2) #return positions of two ends
	})
	data.points=do.call(rbind,data.points)
	if(! is.null(color.col)){
		link.colors <- Data[,color.col]
	}else{
		link.colors=rep(rcPar$color.line,nrow(Data))
	}
	if(! is.null(data.col)){
		lwd <- Data[,data.col] *  max.lwd / max(Data[,data.col])
	}else{
		lwd=rep(1,nrow(Data))
	}
	swap=data.points[, 1]>data.points[, 2] #whether to swap start and end
	data.points[swap,c(1,2)]=data.points[swap,c(2,1)]
	P0 <- do.call(cbind,rc.get.coordinates(track.id,Pos=data.points[,1],innerSide=FALSE))
	P2 <- do.call(cbind,rc.get.coordinates(track.id,Pos=data.points[,2],innerSide=FALSE))
	bc.point.num = 1000; tx=seq(0, 1, length.out=bc.point.num); mtx2=(1 - tx)^2; tx2=tx^2
	for (i in 1:nrow(data.points)) {
		links <- rc.link.line(P0[i,], P2[i,], mtx2=mtx2, tx2=tx2)
		lines(links$pos.x, links$pos.y, type = "l", col = link.colors[i], lwd=lwd[i])
	}
}
rc.link.line=function(P0, P2, mtx2, tx2){
	link.x <- mtx2 * P0[1] + tx2 * P2[1]
	link.y <- mtx2 * P0[2] + tx2 * P2[2]
	return(list(pos.x = link.x, pos.y = link.y))
}
#if multiple links originate from the same chromosome/module, the links can be ordered to minimize crossing by re-assigning their positions
rc.link.sort=function(Data){
	#Data is a data.frame with at least 4 columns: Chr1, Pos1, Chr2, and Pos2
	chromPar=rc.get.chrom()
	order1=sapply(Data$Chr1,function(x) chromPar[[x]]['Order'])
	order2=sapply(Data$Chr2,function(x) chromPar[[x]]['Order'])
	nChr=length(chromPar)
	for(iCh in unique(c(order1,order2))){
		i1 = order1 == iCh
		i2 = order2 == iCh
		chs = c(order2[i1],order1[i2])
		n=length(chs)
		n1=sum(i1)
		Len=(chromPar[[iCh]]['End']-chromPar[[iCh]]['Start']+1)/n
		#re-assign
		chs[chs < iCh] = nChr + chs[chs < iCh]
		Pos = pmax( round(1 + Len * rank( - chs) - Len/2, 0 ), 1 )
		if(n1>0) Data[i1,'Pos1']=Pos[1:n1]
		if(n-n1>0) Data[i2,'Pos2']=Pos[(1+n1):n]
	}
	Data
}
rc.check.linkData=function(Data,data.col,color.col){
	Data=as.data.frame(Data)
	if(! all(c('Chr1','Pos1','Chr2','Pos2') %in% colnames(Data))) stop('Data is not in proper format\n')
}
