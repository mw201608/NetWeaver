\name{getrankp}
\alias{getrankp}
\title{getrankp}
\description{
  Compute probability (fraction) of values in a large population more extreme than input series.
}
\usage{
  getrankp(x, y, truncated.size=0)
}
\arguments{
 \item{x}{A vector of input series.}
 \item{y}{A vector of population values.}
 \item{truncated.size}{See \code{Details}. Only use when you know this.}
}
\details{
This function can be used to compute permutation-based false discovery rate (two-tailed). In such case, \code{x} contains test statistics from observed data while \code{y} contains test statistics from permutation. \code{x} could be a truncated set, with small values discarded to save memory.
}
\value{
A vector of the same size as \code{x}.
}
\author{
Minghui Wang <m.h.wang@live.com>
}
\examples{
x=abs(rnorm(100))
y=abs(rnorm(10000))
getrankp(x,y)[1:4]
}
