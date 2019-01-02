#To compute composite rank score from multiple variables
#Author: Minghui Wang <m.h.wang@live.com>
#
ensemble_rank=function(x,method=c('ProductOfRank','MeanOfLog','MeanOfLogLog','Mean'),small=1.0e-320,standardize=TRUE){
	method=match.arg(method)
	x=as.matrix(x)
	if(method=='ProductOfRank'){
		G=apply(x,2,function(y) {z=rank(y,ties.method = 'max');M=max(z);(M+1-z)/M} )
		S = exp(apply(log(G), 1, sum))
	}else if(method=='MeanOfLog'){
		G=-log(x+small)
		S=rowSums(G,na.rm=TRUE)/ncol(G)
	}else if(method=='MeanOfLogLog'){
		G=log(-log(x+small)+small)
		G[G<0]=0
		S=rowSums(G,na.rm=TRUE)/ncol(G)
	}else if(method=='Mean'){
		S=rowMeans(abs(x),na.rm=TRUE)
	}
	if(standardize) S=S/max(S)
	S
}
