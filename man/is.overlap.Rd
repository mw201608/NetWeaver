\name{is.overlap}
\alias{is.overlap}
\title{Region overlap test}
\description{
  Test if two regions are overlapping.
}
\usage{
  is.overlap(x,y)
}
\arguments{
 \item{x}{a vector of two numeric values specifying the start and end positions of the first region.}
 \item{y}{a vector of two numeric values specifying the start and end positions of the second region}
}
\value{
 Logic test output.
}
\author{
Minghui Wang <m.h.wang@live.com>
}
\examples{
is.overlap(c(10,100),c(90,120))
is.overlap(c(10,100),c(110,120))
}
