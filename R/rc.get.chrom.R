rc.get.chrom=function(){
	params=rcEnvirInternal[["chromPar"]]
	if(is.null(params)){
		rc.initialize()
		params=rcEnvirInternal[["chromPar"]]
	}
	params
}
