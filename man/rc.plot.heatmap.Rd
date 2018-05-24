\name{rc.plot.heatmap}
\docType{package}
\alias{rc.plot.heatmap}
\title{Plot Heat-map}
\description{
  Add multiple tracks of heat-map on a circos plot.
}
\usage{rc.plot.heatmap(Data, track.id, color.gradient=NULL,
	track.color=NA, track.border=NULL, polygon.border=NULL)}
\arguments{
 \item{Data}{matrix, data to be plotted. See \code{Details}.}
 \item{track.id}{integer, starting track id.}
 \item{color.gradient}{a vector of gradient colors. See \code{details}.}
 \item{track.color}{NA or color code for the track background.}
 \item{track.border}{the color to draw the track border. Use \code{NA} to omit borders.}
 \item{polygon.border}{the color to draw the polygon border. See explanation for \code{\link[graphics]{polygon}}.}
}
\details{
This function employs \code{\link{rc.plot.histogram}} to plot heat-map. Every row of \code{Data} will be plotted as heat-map in one track. The column names of \code{Data} must be the chromosome ids.

If \code{color.gradient} is not \code{NULL}, the data will be scaled to positive integers in the range of 1~length(color.gradient) to index the colors in vector \code{color.gradient}. If \code{color.gradient} is\code{NULL}, the input \code{Data} must be a matrix of colors or anything that can be converted to colors.

}
\author{
Minghui Wang <m.h.wang@live.com>
}
\seealso{\code{\link{rc.plot.histogram}}}
\examples{
##see example of ?Modules
}
