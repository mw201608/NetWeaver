rc.plot.area<-function(size=1,oma=rep(0,4),mar=rep(0,4))
{
	if(size<=0 || size>1) stop('size must be > 0 and no larger than 1')
	rcPar <- rc.get.params()
	plot.new()
	par(oma=oma,mar=mar)
	plot.window(c(-rcPar$radius/size, rcPar$radius/size), c(-rcPar$radius/size, rcPar$radius/size),xaxs='i',yaxs='i')
	return(invisible())
}
