rc.check.cytoband=function(cyto.band){
	if(! all(c('Chr','Start','End') %in% colnames(cyto.band))){
		stop('Chromosome and location must be provided in input cyto.info\n')
	}
	if(! any(c('Stain','BandColor') %in% colnames(cyto.band))){
		stop('Either Stain or BandColor must be provided in input cyto.info\n')
	}
	return(invisible())
}
