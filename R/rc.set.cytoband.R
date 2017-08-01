#Set chromosome cytoband color for ideogram plot
rc.set.cytoband=function(cyto.info){
	stainData <- as.character(cyto.info$Stain);
	if(! is.null(cyto.info[["BandColor"]])) return(cyto.info) #skip if customized colors are specified
	band.color <- rep(colors()[652], length(stainData)); #default yellow
	# stains and color index are adapted from RCircos
	stains <- c("gneg", "acen", "stalk", "gvar", "gpos", "gpos100", "gpos75", "gpos66", "gpos50", "gpos33", "gpos25")
	color.index <- c(1, 552, 615, 418, 24, 24, 193, 203, 213, 223, 233)
	for(i in 1:length(stains)){
		band.color[stainData==stains[i]] = colors()[color.index[i]]
	}
	cyto.info[["BandColor"]] = band.color;
	cyto.info
}
