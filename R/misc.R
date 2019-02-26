#test if two regions are overlappng
is.overlap <- function(x,y) max(x) >= min(y) && min(x) <= max(y)
#get rank probability
getrankp <- function(x, y, truncated.size=0){
	if(any(x<0) || any(y<0)) stop('Input must be non-negative\n')
	n1=length(x)
	n2=length(y)
	y=sort(y)
	order.x=order(x)
	rank.x=rep(0L,n1)
	rank.x[order.x]=1:n1
	p=rep(0,n1)
	p[order.x]=n2-findInterval(x[order.x],y)
	p=(p/n2)/((n1+1-rank.x)/(n1+truncated.size))
	p[p>1]=1
	if(n1==1) return(p)
	for(i in 1:(n1-1)) if(p[order.x[i+1]] > p[order.x[i]]){p[order.x[i+1]]=p[order.x[i]]}
	p
}
