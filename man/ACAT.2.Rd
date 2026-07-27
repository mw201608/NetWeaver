\name{ACAT.2}
\alias{ACAT.2}
\title{Combine P-Values with the Aggregated Cauchy Association Test}
\description{
Combines a vector of p-values using a customized aggregated Cauchy association
test (ACAT). Missing values, zeros, and ones receive special handling before
the ACAT statistic is calculated.
}
\usage{
ACAT.2(Pvals, na.action = c("na.omit", "na.to1"), tol = 1.0E-300)
}
\arguments{
\item{Pvals}{A numeric vector of p-values.}
\item{na.action}{How to handle missing p-values: omit them, or replace them
with one.}
\item{tol}{Small positive value used to replace zero p-values when needed.}
}
\value{A numeric combined p-value, or \code{NA} when all input p-values are
missing and \code{na.action = "na.omit"}.}
\examples{
ACAT.2(c(0.01, 0.05, 0.20))
}
