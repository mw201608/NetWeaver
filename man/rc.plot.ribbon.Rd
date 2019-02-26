\name{rc.plot.ribbon}
\docType{package}
\alias{rc.plot.ribbon}
\title{Plot Ribbon}
\description{
  Add a track of ribbons on circos plot.
}
\usage{rc.plot.ribbon(Data, track.id, color.col = NULL, twist=FALSE)}
\arguments{
 \item{Data}{data.frame of at least six columns. See \code{Details}.}
 \item{track.id}{integer, the track number for plotting.}
 \item{color.col}{integer or character string, specifies the column of colors.}
 \item{twist}{logic, whehter to twist the ribbon.}
}
\details{
 \code{Data} must have at least six columns: \code{Chr1}, \code{Start1}, \code{End1}, \code{Chr2}, \code{Start2} and \code{End2}, specifying the chromosomes and positions for the start and end points of the ribbons. Additional columns may be used to specify the color.
}
\author{
Minghui Wang <m.h.wang@live.com>
}
\seealso{\code{\link{rc.plot.histogram}}, \code{\link{rc.plot.link}}}
\examples{
#This is not to be run alone. Please see tutorial vignette("netweaver") for usage.
}
