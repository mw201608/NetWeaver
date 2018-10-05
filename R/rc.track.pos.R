#retrieve the inner and outer radius of a track
rc.track.pos<-function(track.id)
{
	rcPar <- rc.get.params()
	#track height including padding
	track.height <- rcPar$track.height * ( 1 + rcPar$track.padding);
	if(rcPar$Layout=='circular'){
		out.pos <- rcPar$radius - (track.id-1) * track.height;
		in.pos    <- out.pos - rcPar$track.height;
		return (c(out.pos=out.pos, in.pos=in.pos));
	}
	bottom.pos <- (track.id-1) * track.height;
	up.pos <- bottom.pos + rcPar$track.height;
	return (c(bottom.pos=bottom.pos, up.pos=up.pos));
}
