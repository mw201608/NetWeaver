\name{rc.plot.ideogram}
\docType{package}
\alias{rc.plot.ideogram}
\title{Plot Ideogram}
\description{
  Add chromosome ideogram on circos plot.
}
\usage{rc.plot.ideogram(track.ids, plot.band=TRUE, plot.chromosome.id=TRUE, 
	chrom.alias=NULL, color.chromosome.id=NULL, cex.text=1, track.border=NULL,
	polygon.border=NULL, las=0, custom.track.height=NULL)}
\arguments{
 \item{track.ids}{vector, two integers specifying the two tacks for plotting chromosome name and cyto band respectively.}
 \item{plot.band}{logic, whether to plot ideogram cyto band.}
 \item{plot.chromosome.id}{logic, whether to plot chromosome id.}
 \item{chrom.alias}{NULL or a chromosome named vector of alias.}
 \item{color.chromosome.id}{NULL or a chromosome named vector of colors.}
 \item{cex.text}{numeric, scale of text.}
 \item{track.border}{the color to draw the track border. Use NA to omit borders.}
 \item{polygon.border}{the color to draw the polygon border. See explanation for \code{\link[graphics]{polygon}}.}
 \item{las}{numeric in {0,1,2,3}; the style of chromosome labels. 0, always parallel to the track [default]; 1, always horizontal; 2, always perpendicular to the track; 3, always vertical.}
 \item{custom.track.height}{NULL or numeric, specifying customized track height when plotting cyto band.}
}
\author{
Minghui Wang <m.h.wang@live.com>
}
\examples{
#This is not to be run alone. Please see tutorial vignette("netweaver") for usage.
}
