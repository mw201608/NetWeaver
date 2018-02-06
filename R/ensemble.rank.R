#To compute composite rank score from multiple variables
#Author: Minghui Wang <m.h.wang@live.com>
#
ensemble_rank=function(x,method=c('ProductOfRank','MeanOfLog','MeanOfLogLog'),small=1.0e-320){
#input:
# x is a matrix of discriminant values (e.g., P values) measuring the strength of the association between objects (eg modules) (in rows) and variables/features (in columns)
# method, a character string specifyign the ranking metric
# small, offset the p values
#output:
# a vector of ranking scores between 0 to 1
#
#Three methods are currently implemented:
#ProductOfRank is the one used in Zhang et al. (2013) Cell 153: 707-720; a slight change is made in the present metric such that the module rank scores are between 0 and 1. 
#MeanOfLog computes the mean of -log(p value) which penalizes insignificant p values
#MeanOfLogLog computes the mean of log(-log(p value)) which penalizes insignificant p values and shrinks the difference in the ranges of p values
	method=match.arg(method)
	x=as.matrix(x)
	if(method=='ProductOfRank'){
		G=apply(x,2,function(y) {z=rank(y,ties.method = 'max');M=max(z);(M+1-z)/M} )
		S=apply(G,1, prod)
		S/max(S)
	}else if(method=='MeanOfLog'){
		G=-log(x+small)
		S=rowSums(G,na.rm=TRUE)/ncol(G)
		S/max(S)
	}else if(method=='MeanOfLogLog'){
		G=log(-log(x+small)+small)
		G[G<0]=0
		S=rowSums(G,na.rm=TRUE)/ncol(G)
		S/max(S)
	}
}
