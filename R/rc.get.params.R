rc.get.params=function(){
	params=rcEnvirInternal[["rcParams"]]
	if(is.null(params)){
		rc.initialize()
		params=rcEnvirInternal[["rcParams"]]
	}
	params
}
#
rc.params.default=function(...){
#..., a vector.list of named items
#Return a list of parameters
	params=as.list(c(...))
	#
	params$default.tracks=4
	params$default.track.height=0.15
	params$default.radius=1
	#
	if(is.null(params$color.line)) params$color.line="black"
	if(is.null(params$color.hist)) params$color.hist="red"
	if(is.null(params$chr.padding)) params$chr.padding=0.1 #padding between chromsomes is a fraction of the total chromosome sizes
	if(is.null(params$track.padding)) params$track.padding=0.1 #padding between tracks is a fraction of the track height
	if(is.null(params$num.tracks)) params$num.tracks = params$default.tracks
	if(is.null(params$track.height)) params$track.height=params$default.track.height
	if(is.null(params$radius)) params$radius = params$default.radius
	#
	params
}
#calculate chromosome settings
rc.compute.chromPar=function(cyto.info,chr.order=NULL,chr.padding=0.01){
	chrom=lapply(split(cyto.info,cyto.info$Chr),function(d){
		c(min(d[,'Start']),max(d[,'End']))
	})
	if(is.null(chr.order)) chr.order=unique(cyto.info$Chr)
	if(! (all(chr.order %in% names(chrom)))) stop('Not all elements of chr.order are present in cyto.info\n')
	chrom=chrom[chr.order]
	chrom=do.call(rbind,chrom)
	chrom=data.frame(Chr=chr.order,Start=chrom[,1],End=chrom[,2],stringsAsFactors=FALSE)
	nChr=nrow(chrom)
	totalSize=sum(cyto.info[,'End']-cyto.info[,'Start']+1)
	chrgap=floor(totalSize*chr.padding/nChr) #gap between chromosomes
	totalLen = totalSize + chrgap * nChr
	chromPar=list()
	size=0
	for(i in 1:nChr){
		Chr=chrom[i,'Chr']
		chromPar[[length(chromPar)+1]]=c(Start=chrom[i,'Start'],End=chrom[i,'End'],cumStart=size,Order=i)
		size=size+chrom[i,'End']-chrom[i,'Start']+1+chrgap
		names(chromPar)[length(chromPar)]=Chr
	}
	list(chromPar=chromPar,totalLen=totalLen)
}
