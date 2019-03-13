rc.initialize=function(cyto.info,num.tracks=NULL,chr.order=NULL,stepUnit=10^7,Layout=c('circular','landscape'),params=list()){
	Layout=match.arg(Layout)
	rc.check.cytoband(cyto.info)
	params <- rc.params.default(params)
	params$Layout <- Layout
	#
	if(!is.null(num.tracks)){
		num.tracks=as.integer(num.tracks)
		if(num.tracks > params$default.tracks){
			params$num.tracks=num.tracks
			if(params$Layout=='circular') params$radius = params$default.radius + params$track.height * (1+params$track.padding) * (num.tracks - params$default.tracks)
			if(params$Layout=='landscape'){
				params$track.height = params$default.radius / params$num.tracks / (1+params$track.padding)
			}
		}
	}
	#
	cPar=rc.compute.chromPar(cyto.info,chr.order=chr.order,chr.padding=params$chr.padding)
	#
	rcEnvirInternal[["cyto.info"]] <- rc.set.cytoband(cyto.info) #set cyto band color
	rcEnvirInternal[["rcParams"]] <- params
	rcEnvirInternal[["chromPar"]] <- cPar$chromPar
	#stepSize, the size of move along the chromosomes when plotting
	rcEnvirInternal[["baseUnits"]] <- list(halfPi=pi/2,slice.size=params$slice.size,slice.rotate=params$slice.rotate,unitDegree=params$slice.size/cPar$totalLen,unitLenX=params$default.x.length/cPar$totalLen,totalChrLength=cPar$totalLen,stepSize=ceiling(cPar$totalLen/stepUnit))
	return(invisible())
}
