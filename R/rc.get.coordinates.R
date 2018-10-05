#get x,y-coordinates of the two rings for a given chromosome location/fragment in a track
rc.get.trackCoordinates=function(track.id,Start,End,Chr=NULL,degree=NULL,trackThickness=NULL){
#track.id, number of track from outermost
#Chr, chromosome
#Start/End, position on the chromosome
#return a list of x and y coordinates
	rcPar=rc.get.params()
	track.pos = rc.track.pos(track.id)
	if(is.null(trackThickness)) trackThickness=rc.get.params()$track.height
	if(rcPar$Layout=='circular'){
		if(is.null(degree)){
			apos=0
			if(! is.null(Chr)) apos=rc.get.cumLoc(Chr,0)
			cumStart=Start+apos-1
			cumEnd=End+apos
			#
			baseUnits=rc.get.baseUnits()
			cumLoc=seq(cumStart,cumEnd,by=baseUnits$stepSize)
			degree=rc.compute.degree(cumLoc)
		}else{
			degree=degree*pi/180
		}
		xCos=cos(degree)
		ySin=sin(degree)
		#
		
		inner.location=track.pos['in.pos']
		outer.location=inner.location+trackThickness
		#
		pos.x <- c(xCos*outer.location, rev(xCos)*inner.location);
		pos.y <- c(ySin*outer.location, rev(ySin)*inner.location);
		return(list(x=pos.x,y=pos.y))
	}
	pos.x <- c(rc.compute.x.landscape(rc.get.cumLoc(Chr,Start)),rc.compute.x.landscape(rc.get.cumLoc(Chr,End)))
	pos.y <- c(track.pos['bottom.pos'],track.pos['bottom.pos']+trackThickness)
	return(list(x=c(pos.x,rev(pos.x)),y=rep(pos.y,each=2)))
}
rc.get.cumLoc=function(Chr,pos){
	chromPar=rc.get.chrom()
	if(length(Chr)>1){
		if(length(Chr)!=length(pos)) stop('Vector Chr has a different length from vector pos\n')
		for(i in 1:length(Chr)){
			iChr=chromPar[[Chr[i]]]
			pos[i] = pos[i] + iChr['cumStart'] - iChr['Start'] + 1
		}
	}else{
		iChr=chromPar[[Chr]]
		pos = pos + iChr['cumStart'] - iChr['Start'] + 1
	}
	pos
}
#compute the coordinates of a cumulative location in a track
rc.get.coordinates=function(track.id,Pos,Chr=NULL,degree=NULL,innerSide=TRUE,bottomSide=TRUE){
	rcPar=rc.get.params()
	track.pos = rc.track.pos(track.id)
	if(rcPar$Layout=='circular'){
		radius=track.pos[ifelse(innerSide,'in.pos','out.pos')]
		if(is.null(degree)){
			if(is.null(Pos)) stop('Either degree or Pos must be specified.\n')
			cumLoc=Pos
			if(! is.null(Chr)) cumLoc=rc.get.cumLoc(Chr,Pos)
			degree=rc.compute.degree(cumLoc)
		}else{
			degree=degree*pi/180
		}
		xCos=cos(degree)
		ySin=sin(degree)
		pos.x <- xCos*radius;
		pos.y <- ySin*radius;
		return(list(x=pos.x,y=pos.y))
	}
	if(is.null(Chr) || is.null(Pos)) stop('Both Chr and Pos must be specified.\n')
	pos.x=rc.compute.x.landscape(rc.get.cumLoc(Chr,Pos))
	pos.y=track.pos[ifelse(bottomSide,'bottom.pos','up.pos')]
	return(list(x=pos.x,y=pos.y))
}
#compute the degree of cumulative locations
rc.compute.degree=function(cumLoc){
#cumLoc, a vector of cumulative locations since first chromosome
	baseUnits=rc.get.baseUnits()
	degree = baseUnits$halfPi - cumLoc*baseUnits$unitDegree
	degree
}
#compute the x of cumulative locations for landscape Layout
rc.compute.x.landscape=function(cumLoc){
#cumLoc, a vector of cumulative locations since first chromosome
	baseUnits=rc.get.baseUnits()
	baseUnits$unitLenX * cumLoc
}
