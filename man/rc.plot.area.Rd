\name{rc.plot.area}
\docType{package}
\alias{rc.plot.area}
\title{Setup Plot Area}
\description{
 Create new graphics frame for circle plot .
}
\usage{rc.plot.area(size=1, oma=rep(0,4), mar=rep(0,4))}
\arguments{
 \item{size}{a value between 0 to 1, specifying the effective size of the circle plot area in the current window. The smaller the \code{size}, the larger the blank area around the circle plot.}
 \item{oma}{A vector of the form \code{c(bottom, left, top, right)} giving the size of the outer margins in lines of text.}
 \item{mar}{A numerical vector of the form \code{c(bottom, left, top, right)} which gives the number of lines of margin to be specified on the four sides of the plot.}
}
\details{
	After setting up, the extremes of the user coordinates of the plotting region can be found out by \code{par('usr')}.
}
\author{
Minghui Wang <m.h.wang@live.com>
}
\seealso{\code{\link{rc.initialize}}, \code{\link{rc.reset.params}}}
\examples{
##see example of rc.initialze() & ?Modules
}
