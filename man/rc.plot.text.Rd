\name{rc.plot.text}
\docType{package}
\alias{rc.plot.text}
\title{Plot Text}
\description{
  Plot text labels at designated position.
}
\usage{rc.plot.text(Data, track.id, col='black', custom.track.height=NULL, ...)}
\arguments{
 \item{Data}{a data.frame. See \code{Details}.}
 \item{track.id}{a vector of integers, specifying the tracks for plotting text.}
 \item{col}{a vector of color for the text.}
 \item{custom.track.height}{NULL or numeric, specifying customized track height.}
 \item{...}{further graphical parameters (from \code{\link{par}}), such as srt and family.}
}
\details{
\code{Data} is a data.frame with at least three columns named 'Chr', 'Pos', and 'Label', specifying the chrosomal positoin and text labels.
}
\author{
Minghui Wang <m.h.wang@live.com>
}
\seealso{\code{\link{rc.plot.histogram}}, \code{\link{rc.plot.track}}}
\examples{
##see example of rc.initialze() & ?Modules
}
