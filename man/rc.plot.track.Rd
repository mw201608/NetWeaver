\name{rc.plot.track}
\docType{package}
\alias{rc.plot.track}
\title{Plot Track}
\description{
  Add a track to a circos plot.
}
\usage{
rc.plot.track(track.id, border='black', col=NA, custom.track.height=NULL)
}
\arguments{
 \item{track.id}{integer, track id.}
 \item{border}{the color to draw the track border. Use NA to omit borders.}
 \item{col}{NA or color code for the track background.}
 \item{custom.track.height}{NULL or numeric, specifying customized track height. See \code{details}.}
}
\details{
\code{custom.track.height} sets customized track height, which will be useful for track(s) requiring bigger/smaller height than default.
}
\author{
Minghui Wang <m.h.wang@live.com>
}
\seealso{\code{\link{rc.plot.histogram}}, \code{\link{rc.plot.mHistogram}}}
\examples{
##see example of rc.initialze() & ?Modules
}
