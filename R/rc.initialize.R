rc.initialize=function(cyto.info,num.tracks=NULL,chr.order=NULL,stepUnit=10^7,params=list()){
	rc.check.cytoband(cyto.info)
	params <- rc.params.default(params)
	#
	if(!is.null(num.tracks)){
		num.tracks=as.integer(num.tracks)
		if(num.tracks > params$default.tracks){
			params$num.tracks=num.tracks
			params$radius = params$default.radius + params$track.height * (1+params$track.padding) * (num.tracks - params$default.tracks)
		}
	}
	#
	cPar=rc.compute.chromPar(cyto.info,chr.order=chr.order,chr.padding=params$chr.padding)
	#
	rcEnvirInternal[["cyto.info"]] <- rc.set.cytoband(cyto.info) #set cyto band color
	rcEnvirInternal[["rcParams"]] <- params
	rcEnvirInternal[["chromPar"]] <- cPar$chromPar
	#stepSize, the size of move along the chromosomes when plotting
	rcEnvirInternal[["baseUnits"]] <- list(halfPi=pi/2,unitDegree=2*pi/cPar$totalLen,totalChrLength=cPar$totalLen,stepSize=ceiling(cPar$totalLen/stepUnit))
	return(invisible())
}
