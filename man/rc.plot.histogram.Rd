\name{rc.plot.histogram}
\docType{package}
\alias{rc.plot.histogram}
\title{Plot Histogram, Barplot or Heat-map}
\description{
  Add a track of histogram, barplot, or heat-map on a circos plot.
}
\usage{rc.plot.histogram(Data, track.id, data.col=NULL, color.col=NULL,
 color.gradient=NULL, fixed.height=FALSE, track.color=NA, track.border=NULL,
 custom.track.height=NULL, max.value=NULL)}
\arguments{
 \item{Data}{data.frame, data to be plotted. See \code{Details}.}
 \item{track.id}{integer, in which track to plot.}
 \item{data.col}{integer, specifying the column that contains the data for plotting.}
 \item{color.col}{integer, specifying the column that contains the color.}
 \item{color.gradient}{a vector of gradient colors. See \code{details}.}
 \item{fixed.height}{logical, whether to fix the histogram height which results in heat-map. \code{TRUE} if \code{data.col} is \code{NULL}.}
 \item{track.color}{NA or color code for the track background.}
 \item{track.border}{the color to draw the track border. Use \code{NA} to omit borders.}
 \item{custom.track.height}{NULL or numeric, specifying customized track height. See \code{details}.}
 \item{max.value}{NULL or numeric, specifying the maximum data value for normalization. Default NULL, get max from data column \code{data.col}.}
}
\details{
\code{Data} must have at least three columns. The first three columns must be named as \code{Chr}, \code{Start} and \code{End}, specifying the chromosomes and positions for the start and end points of the links. Additional columns can be used to specify data and color.

If \code{color.gradient} is not \code{NULL}, \code{color.col} is ignored and the data will be scaled to positive integers in the range of 1~length(color.gradient) to index the colors in vector \code{color.gradient}.

\code{custom.track.height} bypasses default track height, which will be useful for track(s) requiring bigger/smaller height than default. The default track height can be retrived by function \code{\link{rc.get.params}} after running \code{\link{rc.initialize}}.
}
\author{
Minghui Wang <m.h.wang@live.com>
}
\seealso{\code{\link{rc.plot.link}}, \code{\link{rc.plot.mHistogram}}}
\examples{
##see example of rc.initialze() & ?Modules
}
