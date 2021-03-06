\name{rc.get.coordinates}
\docType{package}
\alias{rc.get.coordinates}
\alias{rc.get.trackCoordinates}
\title{Get Coordinates}
\description{
  Retrieve x and y coordinates in a track.
}
\usage{
rc.get.trackCoordinates(track.id, Start, End, Chr=NULL, degree=NULL, trackThickness=NULL)
rc.get.coordinates(track.id, Pos, Chr=NULL, degree=NULL, innerSide=TRUE, bottomSide=TRUE,
	adjust.origin=TRUE)
}
\arguments{
 \item{track.id}{number of track from outermost.}
 \item{Start, End}{start/end position on a chromosome or since first chromosome. See \code{Details}.}
 \item{Pos}{a vector of positions on a chromosome or since first chromosome. See \code{Details}.}
 \item{Chr}{a chromosome id. See \code{Details}.}
 \item{degree}{the angle of the arc rotation, overwrites \code{Start}, \code{End}, \code{Pos} and \code{Chr}.}
 \item{trackThickness}{thickness (height) of a track.}
 \item{innerSide}{whether to compute coordinates on the inner side of a track. Outer side coordinates will be returned if FALSE. Used for \code{circular} layout.}
 \item{bottomSide}{whether to compute coordinates on the bottom side of a track. Upper side coordinates will be returned if FALSE. Used for \code{landscape} layout.}
 \item{adjust.origin}{whether to relocate origin when plotting only a slice/section of the full 360 degree circle. Used internally for \code{circular} layout.}
}
\details{
If \code{Chr} is \code{NULL}, \code{Start}, \code{End} and \code{Pos} will be considered as cumulative positions since first chromosome.
}
\value{
 A list of x and y coordinates.
}
\author{
Minghui Wang <m.h.wang@live.com>
}
\seealso{\code{\link{rc.initialize}}, \code{\link{rc.reset.params}}}
\examples{
#This is not to be run alone. Please see tutorial vignette("netweaver") for usage.
}
