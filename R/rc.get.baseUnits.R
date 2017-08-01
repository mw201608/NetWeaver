#retrieve unit scale in a circle
rc.get.baseUnits=function(){
	params=rcEnvirInternal[["baseUnits"]]
	if(is.null(params)){
		rc.initialize()
		params=rcEnvirInternal[["baseUnits"]]
	}
	params
}
