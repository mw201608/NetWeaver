\name{rc.plot.link}
\docType{package}
\alias{rc.plot.link}
\title{Plot Link}
\description{
  Add a track of links on circos plot.
}
\usage{rc.plot.link(Data, track.id, data.col=NULL, color.col = NULL,
 max.lwd=1, sort.links=TRUE, arrow.length=0, arrow.angle=30, ...)}
\arguments{
 \item{Data}{data.frame of at least four columns. See \code{Details}.}
 \item{track.id}{integer, the track number for plotting.}
 \item{data.col}{integer or character string, specifies the column of data.}
 \item{color.col}{integer or character string, specifies the column of colors.}
 \item{max.lwd}{integer, maximum line width.}
 \item{sort.links}{logical, whether to re-assign link start and end positions by sorting them. See \code{details}}
 \item{arrow.length}{length of the edges of the arrow head (in inches). See \code{\link{arrows}}}
 \item{arrow.angle}{angle from the shaft of the arrow to the edge of the arrow head. See \code{\link{arrows}}}
 \item{...}{further graphical parameters (from \code{\link{par}}), such as lty and lwd.}
}
\details{
\code{Data} must have at least four columns: \code{Chr1}, \code{Pos1}, \code{Chr2} and \code{Pos2}, specifying the chromosomes and positions for the start and end points of the links. Additional columns may be used to specify the data, color and max line width.

If \code{data.col} is not NULL, the line width will be proportional to the values in column \code{data.col} with the maximum line width determined by \code{max.lwd}.

If \code{sort.links} is true, the links originate from the same chromosome will be sorted and equally spaced to minimize crossing.
}
\author{
Minghui Wang <m.h.wang@live.com>
}
\seealso{\code{\link{rc.plot.histogram}}, \code{\link{rc.plot.ribbon}}}
\examples{
#This is not to be run alone. Please see tutorial vignette("netweaver") for usage.
}
