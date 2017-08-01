\name{rc.plot.track.id}
\docType{package}
\alias{rc.plot.track.id}
\title{Plot Track Id}
\description{
  Plot labels in designated tracks.
}
\usage{rc.plot.track.id(track.id, labels=NULL, degree=0,
	col='black', custom.track.height=NULL, ...)}
\arguments{
 \item{track.id}{a vector of integers, specifying the tracks for plotting id.}
 \item{labels}{NULL or a vector of character string, specifying the text to be written.}
 \item{degree}{the angle of the arc rotation.}
 \item{col}{color for the text.}
 \item{custom.track.height}{NULL or numeric, specifying customized track height.}
 \item{...}{further graphical parameters (from par), such as srt and family.}
}
\details{
If \code{labels} is NULL, values of \code{track.id} will be used as text labels.
}
\author{
Minghui Wang <m.h.wang@live.com>
}
\seealso{\code{\link{rc.plot.histogram}}, \code{\link{rc.plot.track}}}
\examples{
##see example of rc.initialze() & ?Modules
}
