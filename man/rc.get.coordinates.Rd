\name{rc.get.coordinates}
\docType{package}
\alias{rc.get.coordinates}
\alias{rc.get.ringCoordinates}
\title{Get Coordinates}
\description{
  Retrieve x and y coordinates in a track.
}
\usage{
rc.get.ringCoordinates(track.id, Start, End, Chr=NULL, degree=NULL, ringThickness=NULL)
rc.get.coordinates(track.id, Pos, Chr=NULL, degree=NULL, innerSide=TRUE)
}
\arguments{
 \item{track.id}{number of track from outermost.}
 \item{Start, End}{start/end position on a chromosome or since first chromosome. See \code{Details}.}
 \item{Pos}{a vector of positions on a chromosome or since first chromosome. See \code{Details}.}
 \item{Chr}{a chromosome id. See \code{Details}.}
 \item{degree}{the angle of the arc rotation, overwrites \code{Start}, \code{End}, \code{Pos} and \code{Chr}.}
 \item{ringThickness}{thickness of a ring.}
 \item{innerSide}{whether to compute coordinates on the inner side of a track. Outer side coordinates will be returned if FALSE.}
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
##not run###
}
