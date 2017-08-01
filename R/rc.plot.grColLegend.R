rc.plot.grColLegend=function(x,y,cols,at=NULL,legend=at,title='Color',width=0.1,height=0.3,gap=0,direction=c('vertical','horizontal'),cex.text=1,cex.title=1){
	direction=match.arg(direction)
	nCol=length(cols)
	if(is.null(at)) at=unique(c(1,ceiling(nCol/2),nCol))
	if(is.null(legend)) legend=at
	if(direction=='vertical'){
		vunit=(height-gap*(nCol-1))/nCol
		if(vunit<=0) stop('gap is too big')
		for(i in 1:nCol){
			polygon(c(x,x+width,x+width,x),c(y+(gap+vunit)*(i-1),y+(gap+vunit)*(i-1),y+gap*(i-1)+vunit*i,y+gap*(i-1)+vunit*i),col=cols[i],border=NA)
		}
		text(x+width/2,y+height,labels=title,pos=3,cex=cex.title)
		for(j in 1:length(at)){
			text(x+width,y+gap*(at[j]-1)+vunit*(at[j]-0.5),labels=legend[j],pos=4,cex=cex.text)
			lines(c(x+width,x+width*1.2),rep(y+gap*(at[j]-1)+vunit*(at[j]-0.5),2))
		}
	}else{
		hunit=(width-gap*(nCol-1))/nCol
		if(hunit<=0) stop('gap is too big')
		for(i in 1:nCol){
			polygon(c(x+(gap+hunit)*(i-1),x+gap*(i-1)+hunit*i,x+gap*(i-1)+hunit*i,x+(gap+hunit)*(i-1)),c(y,y,y+height,y+height),col=cols[i],border=NA)
		}
		text(x+width/2,y+height,labels=title,pos=3,cex=cex.title)
		for(j in 1:length(at)){
			text(x+gap*(at[j]-1)+hunit*(at[j]-1),y,labels=legend[j],pos=1,cex=cex.text)
			lines(rep(x+gap*(at[j]-1)+hunit*(at[j]-0.5),2),c(y, y-height/5))
		}
	}
}
