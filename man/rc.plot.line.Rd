\name{rc.plot.line}
\docType{package}
\alias{rc.plot.line}
\title{Plot Line}
\description{
  Plot lines at designated position.
}
\usage{rc.plot.line(Data, track.id, color.col=NULL, custom.length=NULL, ...)}
\arguments{
 \item{Data}{a data.frame. See \code{Details}.}
 \item{track.id}{a vector of integers, specifying the tracks for plotting line.}
 \item{color.col}{integer, specifying the column that contains the color.}
 \item{custom.length}{\code{NULL} or numeric, specifying line length.}
 \item{...}{further graphical parameters (from \code{\link{par}}), such as lty and lwd.}
}
\details{
\code{Data} is a data.frame, with at least two columns named 'Chr' and 'Pos', specifying the chromosomal positoins for each line in every row. An additional column can be used to specify the line color. Default line length is determined by track height.
}
\author{
Minghui Wang <m.h.wang@live.com>
}
\seealso{\code{\link{rc.plot.histogram}}, \code{\link{rc.plot.track}}}
\examples{
##see example of rc.initialze() & ?Modules
}
