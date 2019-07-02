rc.plot.sunburst=function(Data, root=NULL, color.vector=NULL, rect.color.func=function(n=20) rev(heat.colors(n)), rect.data=NULL, rect.data.cutoff=NULL, rect.data.min=NULL,rect.data.max=NULL,
polygon.border=NULL, show.label=FALSE, show.label.selected=NULL,show.legend=TRUE,
legend.x=0.8,legend.y=0.9,legend.width=0.1,legend.height=0.3,legend.title='Color',legend.cex.text=1,legend.cex.title=1.2,legend.direction='vertical',rotate=0,highlight.ids=NULL){
	stopifnot(is.data.frame(Data))
	colnames(Data)[1:2]=c('child','parent')
	if(is.null(root)){
		root <- Data$parent[!Data$parent %in% Data$child[Data$parent != Data$child]]
		root=unique(root)
	}else{
		all1=unique(root)
		while(TRUE){
			all2=union(all1,Data$child[Data$parent %in% all1])
			if(length(all1)==length(all2)) break
			all1=all2
		}
		Data=Data[Data$parent %in% all1,]
	}
	#
	nodes=union(Data$parent,Data$child)
	nodes=data.frame(Node=nodes,Leaf=(!nodes %in% Data$parent),stringsAsFactors = FALSE)
	nodes$DS=1
	rownames(nodes)=nodes$Node
	d1=NA
	if(!is.null(rect.data)){
		d1=Data[match(nodes$Node,Data$child),rect.data]
		if(is.null(rect.data.min)){
			rect.data.min <- min(d1,na.rm=TRUE)
			rect.data.min <- floor(rect.data.min)
		}
		if(is.null(rect.data.max)){
			rect.data.max <- max(d1,na.rm=TRUE)
			rect.data.max <- ceiling(rect.data.max)
		}
		d1[d1<rect.data.min]=rect.data.min
		d1[d1>rect.data.max]=rect.data.max
	}
	if(!is.null(color.vector)){
		nodes=data.frame(nodes,color.col=color.vector[nodes$Node],rect.data=d1,stringsAsFactors = FALSE)
	}else{
		if(is.null(rect.data)){
			nodes=data.frame(nodes,color.col=sample(rect.color.func(nrow(nodes))),rect.data=NA,stringsAsFactors = FALSE)
		}else{
			if(is.numeric(Data[,rect.data])){
				ncolors=length(rect.color.func())
				d2=ceiling((d1-rect.data.min)*ncolors/(rect.data.max-rect.data.min))
				d2[which(d2==0)]=1
				nodes=data.frame(nodes,color.col=rect.color.func()[d2],rect.data=d1,stringsAsFactors = FALSE)
			}else{
				nodes=data.frame(nodes,color.col=rect.color.func()[Data[,rect.data]],rect.data=d1,stringsAsFactors = FALSE)
			}
		}
	}
	Data=Data[Data$parent != Data$child,]
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
	if(any(is.na(Data$layer))) stop('Orphan leaf node(s) found, probably due to invalid setting of root node.\n')
	#compute DS size
	for(iLayer in nLayer:2){
		i = which(Data$layer==iLayer)
		c1=tapply(nodes[Data$child[i],'DS'],Data$parent[i],sum)
		nodes[names(c1),'DS']=c1
	}
	#
	if(!is.null(highlight.ids)){
		highlight.ids=highlight.ids[highlight.ids %in% nodes$Node]
		nhighlight=length(highlight.ids)
		if(nhighlight>0){
			while(TRUE){
				highlight.ids=union(highlight.ids,Data$child[Data$parent %in% highlight.ids])
				n111=length(highlight.ids)
				if(n111==nhighlight) break
				nhighlight=n111
			}
		}
	}else{
		highlight.ids=nodes$Node
	}
	nodes$color.col[!nodes$Node %in% highlight.ids]=NA
	#
	#library(NetWeaver)
	options(stringsAsFactors=FALSE)
	Cyto1=nodes[nodes$Node %in% root,c('Node','DS')]
	colnames(Cyto1)=c('Chr','End')
	Cyto1$Start=1;Cyto1$BandColor=nodes[Cyto1$Chr,'color.col'];Cyto1$Layer=1
	rownames(Cyto1)=Cyto1$Chr
	#
	rc.initialize(Cyto1, num.tracks=nLayer, params=list(chr.padding=0,track.padding=0,color.hist=NA,slice.rotate=rotate))
	rc.plot.area()
	for(iLayer in 1:nLayer){
		if(iLayer==1){
			rc.plot.histogram(Cyto1,track.id=nLayer-iLayer+2,color.col='BandColor',track.border=NA, polygon.border=polygon.border)
			if(show.label){
				textData=data.frame(Chr=Cyto1$Chr, Pos=(Cyto1$Start+Cyto1$End)/2, Label=rownames(Cyto1),stringsAsFactors=FALSE)
				if(!is.null(show.label.selected)){
					textData=textData[textData$Label %in% show.label.selected,]
				}else{
					if(! is.null(rect.data.cutoff)){
						textData=textData[which(nodes[rownames(Cyto1),'rect.data'] >= rect.data.cutoff),]
					}
				}
				if(nrow(textData)>0) rc.plot.text(textData, track.id=nLayer-iLayer+2.5, cex=0.6)
			}
			next
		}
		i = which(Data$layer==iLayer)
		for(p in unique(Data$parent[i])){
			pc=Data$child[i][Data$parent[i]==p]
			Cyto2=data.frame(Chr=Cyto1[p,'Chr'],End=nodes[pc,'DS'], Start=nodes[pc,'DS'], BandColor=nodes[pc,'color.col'],stringsAsFactors=FALSE)
			Cyto2$Start=Cyto1[p,'Start']+c(0,cumsum(Cyto2$End))[1:length(Cyto2$End)]
			Cyto2$End=Cyto2$Start+Cyto2$End-1
			rownames(Cyto2)=pc
			Cyto2$Layer=iLayer
			Cyto1=rbind(Cyto1,Cyto2)
		}
		Cyto2=Cyto1[Cyto1$Layer==iLayer,]
		rc.plot.histogram(Cyto2,track.id=nLayer-iLayer+2,color.col='BandColor',track.border=NA, polygon.border=polygon.border)
		if(show.label){
			textData=data.frame(Chr=Cyto2$Chr, Pos=(Cyto2$Start+Cyto2$End)/2, Label=rownames(Cyto2),stringsAsFactors=FALSE)
			if(!is.null(show.label.selected)){
				textData=textData[textData$Label %in% show.label.selected,]
			}else{
				if(! is.null(rect.data.cutoff)){
					textData=textData[which(nodes[rownames(Cyto2),'rect.data'] >= rect.data.cutoff),]
				}
			}
			if(nrow(textData)>0) rc.plot.text(textData, track.id=nLayer-iLayer+2.5, cex=0.6)
		}
	}
	if(show.legend){
		if(is.null(color.vector) && ! is.null(rect.data)){
			cols=rect.color.func()
			if(is.numeric(Data[,rect.data])){
				rc.plot.grColLegend(x=legend.x, y=legend.y, cols=cols, at=c(1,floor(length(cols)/2),length(cols)),legend=c(rect.data.min,ceiling((rect.data.max+rect.data.min)/2),rect.data.max),
				width=legend.width,height=legend.height,title=legend.title,cex.title=legend.cex.title,cex.text=legend.cex.text,direction=legend.direction)
			}else{
				legend(legend.x,legend.y,legend=names(cols),pch=19,col=cols)
			}
		}
		if(!is.null(color.vector)){
			legend(legend.x,legend.y,legend=names(color.vector),pch=19,col=color.vector)
		}
	}
}
#
rc.sunburst.hierarchy=function(Data, root=NULL){
	stopifnot(is.data.frame(Data))
	colnames(Data)[1:2]=c('child','parent')
	if(is.null(root)){
		root <- Data$parent[!Data$parent %in% Data$child[Data$parent != Data$child]]
		root=unique(root)
		print(root)
	}
	#
	Data=Data[Data$parent != Data$child,]
	findChild=function(x,p,s=NULL){
		if(is.null(s)) s=x
		c1=p$child[p$parent==x]
		if(length(c1)==0) return(s)
		unlist(sapply(c1,findChild,p=p,s=paste0(s,'-',c1)))
	}
	hierarchy=do.call(c,lapply(root,findChild,p=Data))
	#return(hierarchy)
	data.frame(hierarchy=hierarchy,Data[match(sapply(hierarchy,function(x) tail(strsplit(x,'-')[[1]],n=1)),Data$child),],stringsAsFactors=FALSE)
}
