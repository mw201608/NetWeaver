\name{rc.plot.grColLegend}
\docType{package}
\alias{rc.plot.grColLegend}
\title{Plot Gradient Color Legend}
\description{
  Plot gradient color legend at given position.
}
\usage{rc.plot.grColLegend(x, y, cols, at=NULL, legend=at,
	title='Color', width=0.1, height=0.3, gap=0,
	direction=c('vertical', 'horizontal'), cex.text=1, cex.title=1)}
\arguments{
 \item{x, y}{numeric values of coordinates where the legend is plotted. See \code{details}.}
 \item{cols}{a vector of colors.}
 \item{at}{integers; index \code{legend} text in the color vector \code{cols}. See \code{details}.}
 \item{legend}{character strings of legend text with the same length of vector \code{at}.}
 \item{title}{character string of the legend title.}
 \item{width}{numeric, width of the color legend bar.}
 \item{height}{numeric, height of the color legend  bar.}
 \item{gap}{numeric, size of the gap between two color blocks.}
 \item{direction}{plot the color legend bar in either vertical or horizontal direction.}
 \item{cex.text}{numeric, magnitude of the legend text.}
 \item{cex.title}{numeric, magnitude of the legend title.}
}
\details{
	\code{x} and \code{y} set the coordinates of bottom left starting point. The extremes of the user coordinates of the plotting region can be found out by \code{par('usr')}.
	
	By default \code{NULL}, parameter \code{at} will be set as a vector with values 1, \code{ceiling(length(cols)/2)}, and \code{length(cols)}.
}
\author{
Minghui Wang <m.h.wang@live.com>
}
\seealso{\code{\link{rc.plot.link}}, \code{\link{rc.plot.histogram}}}
\examples{
#This is not to be run alone. Please see tutorial vignette("netweaver") for usage.
}
