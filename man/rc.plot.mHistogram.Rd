\name{rc.plot.mHistogram}
\docType{package}
\alias{rc.plot.mHistogram}
\title{Plot Histogram or Barplot Across Chromosomes}
\description{
  Plot histogram or barplot that overlaps with multiple chromosomes.
}
\usage{
rc.plot.mHistogram(Data, track.id, data.col, color.col=NULL, color.gradient=NULL,
 fixed.height=FALSE, track.color=NA, track.border=NULL, polygon.border=NULL,
 custom.track.height=NULL, max.value=NULL)
}
\arguments{
 \item{Data}{data.frame of at least four columns. See \code{Details}.}
 \item{track.id}{integer, the track number for plotting.}
 \item{data.col}{integer or character string, specifies the column of data.}
 \item{color.col}{integer or character string, specifies the column of colors.}
 \item{color.gradient}{a vector of gradient colors.}
 \item{fixed.height}{logical, whether to fix the histogram height, resulting in heat-map if \code{TRUE}.}
 \item{track.color}{NA or color code for the track background.}
 \item{track.border}{the color to draw the track border. Use \code{NA} to omit borders.}
 \item{polygon.border}{the color to draw the polygon border. See explanation for \code{\link[graphics]{polygon}}.}
 \item{custom.track.height}{NULL or numeric, specifying customized track height to overwrite the default. See \code{\link{rc.get.params}} for default value.}
 \item{max.value}{NULL or numeric, specifying the maximum data value for normalization. Default NULL, get max from data column \code{data.col}.}
}
\details{
\code{Data} must have at least four columns: \code{Chr1}, \code{Start1}, \code{Chr2} and \code{End2}, specifying the chromosomes and positions for the start and end points. Additional columns may be used to specify the data, color, etc.\cr\cr
If \code{color.gradient} is not \code{NULL}, it will supersede \code{color.col}.\cr\cr
}
\author{
Minghui Wang <m.h.wang@live.com>
}
\seealso{\code{\link{rc.plot.link}}, \code{\link{rc.plot.histogram}}}
\examples{
##see example of rc.initialze()
}
