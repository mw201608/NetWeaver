rc.reset.params=function(params){
	#validate params
	rcEnvirInternal[["rcParams"]] <- params;
	return(invisible())
}
