rc.plot.sunburst=function(Data, root=NULL, color.vector=NULL, rect.color.func=function(n=20) rev(heat.colors(n)), rect.color.data=NULL, polygon.border=NULL){
	stopifnot(is.data.frame(Data))
	colnames(Data)[1:2]=c('child','parent')
	if(is.null(root)){
		root <- Data$parent[!Data$parent %in% Data$child]
		root=unique(root)
	}
	#
	nodes=union(Data$parent,Data$child)
	nodes=data.frame(Node=nodes,Leaf=(!nodes %in% Data$parent),stringsAsFactors = FALSE)
	nodes$DS=1
	rownames(nodes)=nodes$Node
	if(!is.null(color.vector)){
		nodes=data.frame(nodes,color.col=color.vector[nodes$Node],stringsAsFactors = FALSE)
	}else{
		if(is.null(rect.color.data)){
			nodes=data.frame(nodes,color.col=sample(rect.color.func(nrow(nodes))),stringsAsFactors = FALSE)
		}else{
			d1=Data[match(nodes$Node,Data$child),rect.color.data]
			ncolors=length(rect.color.func())
			d1=ceiling(d1*ncolors/max(d1,na.rm=TRUE))
			d1[which(d1==0)]=1
			nodes=data.frame(nodes,color.col=rect.color.func()[d1],stringsAsFactors = FALSE)
		}
	}
	#
	Data$layer=NA
	nLayer=Data$layer[Data$parent %in% root]=2 #root has layer 1
	while(TRUE){
		i = which(Data$layer==nLayer)
		if(length(i)==0) break
		j = which(Data$parent %in% Data$child[i])
		if(length(j)==0) break
		nLayer=nLayer+1
		Data$layer[j]=nLayer
	}
	if(any(is.na(Data$layer))) stop('Orphan leaf node(s) found probably due to invalid setting of root node.\n')
	#compute DS size
	for(iLayer in nLayer:2){
		i = which(Data$layer==iLayer)
		c1=tapply(nodes[Data$child[i],'DS'],Data$parent[i],sum)
		nodes[names(c1),'DS']=c1
	}
	#
	#library(NetWeaver)
	options(stringsAsFactors=FALSE)
	Cyto1=nodes[nodes$Node %in% root,c('Node','DS')]
	colnames(Cyto1)=c('Chr','End')
	Cyto1$Start=1;Cyto1$BandColor=nodes[Cyto1$Chr,'color.col'];Cyto1$Layer=1
	rownames(Cyto1)=Cyto1$Chr
	#
	rc.initialize(Cyto1, num.tracks=nLayer, params=list(chr.padding=0,track.padding=0,color.hist=NA))
	rc.plot.area()
	for(iLayer in 1:nLayer){
		if(iLayer==1){
			rc.plot.histogram(Cyto1,track.id=nLayer-iLayer+2,color.col='BandColor',track.border=NA, polygon.border=polygon.border)
			next
		}
		i = which(Data$layer==iLayer)
		pp = unique(Data$parent[i])
		for(p in pp){
			pc=Data$child[i][Data$parent[i]==p]
			Cyto2=data.frame(Chr=Cyto1[p,'Chr'],End=nodes[pc,'DS'], Start=nodes[pc,'DS'], BandColor=nodes[pc,'color.col'])
			Cyto2$Start=Cyto1[p,'Start']+c(0,cumsum(Cyto2$End))[1:length(Cyto2$End)]
			Cyto2$End=Cyto2$Start+Cyto2$End-1
			rownames(Cyto2)=pc
			Cyto2$Layer=iLayer
			Cyto1=rbind(Cyto1,Cyto2)
		}
		rc.plot.histogram(Cyto1[Cyto1$Layer==iLayer,],track.id=nLayer-iLayer+2,color.col='BandColor',track.border=NA, polygon.border=polygon.border)
	}
}
