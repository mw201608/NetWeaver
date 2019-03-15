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
	sdeg=90-params$slice.rotate
	edeg=360-seq(0,params$slice.size) + sdeg
	edeg=ifelse(edeg>360,edeg - 360, edeg)
	edeg=edeg*pi/180
	x1=params$radius * cos(edeg)
	y1=params$radius * sin(edeg)
	origin=c(x=(min(x1)+max(x1))/2,y=(min(y1)+max(y1))/2)
	#stepSize, the step size of moving along the chromosomes when plotting
	rcEnvirInternal[["baseUnits"]] <- list(halfPi=pi/2,slice.size.radian=params$slice.size*pi/180,
		slice.rotate.radian=params$slice.rotate*pi/180,unitDegree=params$slice.size*pi/180/cPar$totalLen,
		unitLenX=params$default.x.length/cPar$totalLen,totalChrLength=cPar$totalLen,origin=origin,
		stepSize=ceiling(cPar$totalLen/stepUnit))
	return(invisible())
}
