rc.plot.area<-function(size=1,oma=rep(0,4),mar=rep(0,4))
{
	if(size<=0 || size>1) stop('size must be > 0 and no larger than 1')
	rcPar <- rc.get.params()
	plot.new()
	par(oma=oma,mar=mar)
	if(rcPar$Layout=='circular'){
		plot.window(c(-rcPar$radius/size, rcPar$radius/size), c(-rcPar$radius/size, rcPar$radius/size),xaxs='i',yaxs='i')
	}else{
		plot.window(c(rcPar$default.x.length*(1-1/size)/2, rcPar$default.x.length*(1+1/size)/2), c(rcPar$radius*(1-1/size)/2, rcPar$radius*(1+1/size)/2),xaxs='i',yaxs='i')
	}
	return(invisible())
}
