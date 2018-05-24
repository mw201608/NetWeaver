\name{rc.plot.barchart}
\docType{package}
\alias{rc.plot.barchart}
\title{Bar Plots}
\description{
  Add a track of barcharts.
}
\usage{
rc.plot.barchart(Data, track.id, data.col, color.col=NULL, bar.color=NULL,
	track.color=NA, track.border=NULL, polygon.border=NULL,
	custom.track.height=NULL, ratio=FALSE)
}
\arguments{
 \item{Data}{data.frame, data to be plotted. See \code{Details}.}
 \item{track.id}{integer, in which track to plot.}
 \item{data.col}{integer, specifying the column(s) that contain the data for plotting. A stacked barchart is created if there are multiple columns of data.}
 \item{color.col}{integer, specifying the column that contains the color. Only used when there is only one data column.}
 \item{bar.color}{a vector of colors for the bars or bar components. By default, \code{grey} is used if there is one data column, and a gamma-corrected grey palette if there are multiple columns of data.}
 \item{track.color}{NA or color code for the track background.}
 \item{track.border}{the color to draw the track border. Use \code{NA} to omit borders.}
 \item{polygon.border}{the color to draw the polygon border. See explanation for \code{\link[graphics]{polygon}}.}
 \item{custom.track.height}{NULL or numeric, specifying customized track height to overwrite the default. See \code{\link{rc.get.params}} for default value.}
 \item{ratio}{logical, specifying whether the data values in each row should be converted to ratios. Used only for stacked barchart.}
}
\details{
\code{Data} must have at least three columns. The first three columns must be named as \code{Chr}, \code{Start} and \code{End}, specifying the chromosomes and positions for the start and end points of the links. Additional columns can be used to specify data and color.

\code{\link{rc.plot.histogram}} can also be used for plotting barcharts except stacked barchart.
}
\author{
Minghui Wang <m.h.wang@live.com>
}
\seealso{\code{\link{rc.plot.link}}, \code{\link{rc.plot.mHistogram}}}
\examples{
##see example of rc.initialze() & ?Modules
}
