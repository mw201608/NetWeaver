\name{rc.initialize}
\docType{package}
\alias{rc.initialize}
\title{Initialize Circos Plot Parameters}
\description{
  Setup default parameter for Circos plot.
}
\usage{
rc.initialize(cyto.info, num.tracks=NULL, chr.order=NULL, 
	stepUnit=10^7, Layout=c('circular','landscape'), params=list())
}
\arguments{
 \item{cyto.info}{data.frame, see \code{Details}.}
 \item{num.tracks}{integer, number of tracks.}
 \item{chr.order}{character vector of chromosome ids specifying the ordering of chromosomes.}
 \item{stepUnit}{integer, smoothing factor for faster plotting.}
 \item{Layout}{layout for plotting.}
 \item{params}{a list of named items. See \code{Details}.}
}
\details{
\code{cyto.info} is a data.frame of chromosomal position ordered cytobands, with columns: \code{Chr}, \code{Start}, \code{End}, \code{Stain}, and any additional information (like band color),
where \code{Chr} is chromosome name, \code{Start} and \code{End} are the start and end positions on the chromosome, and \code{Stain} is the cyto stain.
The stain is normally one of the "gneg", "acen", "stalk", "gvar", "gpos", "gpos100", "gpos75", "gpos66", "gpos50", "gpos33", and "gpos25", which will be plotted by color
"white", "red", "steelblue", "lightgrey", "black", "black", "gray40", "gray50", "gray60", "gray70" and "gray80", accordingly. Customized colors for the cyto bands can be specified in
an additional column named "\code{BandColor}".\cr\cr
Additional plot parameters can be specified through argument \code{params}, including:
\itemize{
\item \code{color.line}, color for lines and links, default "black".
\item \code{chr.padding}, padding between chromsomes is a fraction of the total chromosome sizes, default 0.1.
\item \code{track.padding}, paddings between tracks is a fraction of the track height, default 0.1
\item \code{track.height}, track height, default 0.15.
\item \code{radius} radius of the circos, default 1.
\item \code{slice.size}, a value between 0 and \code{2*Pi} (the default) specifying the circular slice size of the circos plot.
\item \code{slice.rotate}, a value between 0 (the default) and \code{2*Pi} specifying the degree to rotate in a clock-wise direction.
}
After initialization, the parameter settings can be retrieved by \code{\link{rc.get.params}}.

Noted that while \code{cyto.info} requires input to be in a form of chromosome cytobands, the input is not limited to genomic features. As illustrated in example data \code{\link{Modules}}, complex features of gene coexpression network modules can also be plotted with the current circos visualization technique.
}
\author{
Minghui Wang <m.h.wang@live.com>
}
\seealso{\code{\link{rc.get.params}}, \code{\link{rc.reset.params}}, \code{\link{rc.plot.ideogram}}, \code{\link{rc.plot.histogram}}, \code{\link{rc.plot.mHistogram}}, \code{\link{rc.plot.barchart}}, \code{\link{rc.plot.link}}, \code{\link{rc.plot.ribbon}}, \code{\link{Modules}}}
\examples{
library(NetWeaver)
options(stringsAsFactors=FALSE)
## set fake Cyto data
Cyto=data.frame(Chr=paste0('C',1:20), Start=1, End=100, BandColor=colors()[2:21])

## initialize circos
#firstly set number of tracks, should be larger than the actual number of tracks
#used for plotting in order to leave sufficient sapce in the middle
num.tracks=9 
#slice size pi/2, rotate pi/6
rc.initialize(Cyto, num.tracks=num.tracks, params=list(chr.padding=0.1,slice.size=0.75*pi,slice.rotate=pi/8))

##retrieve parameters
params=rc.get.params()
#make plot area
rc.plot.area(size=0.95)

##plot ideogram on track 1 and 2 (start from the outside to inside)
track.num=1:2
rc.plot.ideogram(track.num, plot.band=TRUE, plot.chromosome.id=TRUE)

##plot histogram/barchart that span multiple chromosomes
MultHistData=data.frame(Chr1=paste0('C',seq(1,18,3)), Start1=50, 
	Chr2=paste0('C',seq(3,20,3)), End2=20, Col=1:6)
track.num=3
rc.plot.mHistogram(MultHistData, track.id=track.num, data.col=5,
	color.col=5, fixed.height=FALSE)

##plot histogram that occupies two tracks 4 and 5
HistData=data.frame(Chr=paste0('C',1:20), Start=1, End=50, Data=runif(20))
params$color.hist <- 'black'
rc.reset.params(params)
track.num=5
rc.plot.histogram(HistData, track.num, data.col=4, fixed.height=FALSE, 
	track.border=NA, custom.track.height=params$track.height*2)

##plot heatmap on track 6
HeatData=data.frame(Chr=paste0('C',1:20), Start=1,End=100, Data=1:20)
colfuncHeat=function(n) rev(heat.colors(n))
track.num=track.num+1
rc.plot.histogram(HeatData, track.num, data.col=4, color.gradient=colfuncHeat(50),
	fixed.height=TRUE)

##plot stacked barchart on track 7
BarData=data.frame(Chr=paste0('C',1:20), Start=1, 
	End=seq(10,86,length.out=20), Data=matrix(runif(20*4),nrow=20))
track.num=track.num+1
rc.plot.barchart(BarData, track.num, data.col=4:7)

##plot links in the middle
LinkData=data.frame(Chr1=sample(Cyto$Chr,40,replace=TRUE), Pos1=20, 
	Chr2=sample(Cyto$Chr,40,replace=TRUE),Pos2=20, Data=runif(20))
LinkData=LinkData[LinkData$Chr1 != LinkData$Chr2,]
params$color.line='blue'
rc.reset.params(params)
track.num=track.num+1
rc.plot.link(LinkData, track.num, data.col=4, arrow.length=0.1)
ribbonData=data.frame(Chr1=c('C1','C3'), Start1=c(10,10), End1=c(40,40),
	Chr2=c('C17','C10'), Start2=20, End2=60, Col=c('red','brown'))
rc.plot.ribbon(ribbonData, track.num, color.col='Col', twist=TRUE)
#label track id
rc.plot.track.id(2:7, col=2)
#add text label
rc.plot.text(data.frame(Chr='C3',Pos=50,Label='GeneX'), 
	track.id=3,srt=45,cex=0.8,col='blue')
#add line mark
rc.plot.line(data.frame(Chr='C19',Pos=seq(10,90,by=15),Col='red'), 
	track.id=3, color.col=3,arrow.length=0.2)
}
