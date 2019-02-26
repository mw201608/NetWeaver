\name{rc.plot.point}
\docType{package}
\alias{rc.plot.point}
\title{Plot Point}
\description{
  Plot point(s) at designated position.
}
\usage{rc.plot.point(Data, track.id, color.col=NULL, custom.track.height=NULL, ...)}
\arguments{
 \item{Data}{a data.frame. See \code{Details}.}
 \item{track.id}{a vector of integers, specifying the tracks for plotting point(s).}
 \item{color.col}{column id in \code{Data} specifying the colors.}
 \item{custom.track.height}{NULL or numeric, specifying customized track height.}
 \item{...}{further graphical parameters (from \code{\link{par}}).}
}
\details{
\code{Data} is a data.frame with at least two columns named 'Chr', 'Pos', and 'Height', specifying the chrosomal positoin and the height within the track of the points. "\code{Height}" will be sacled to have a maximum of 1.
}
\author{
Minghui Wang <m.h.wang@live.com>
}
\seealso{\code{\link{rc.plot.histogram}}, \code{\link{rc.plot.track}}}
\examples{
#This is not to be run alone. Please see tutorial vignette("netweaver") for usage.
}
