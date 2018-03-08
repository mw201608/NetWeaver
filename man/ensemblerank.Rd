\name{ensemble_rank}
\alias{ensemble_rank}
\title{Ensemble Rank}
\description{
  Compute composite rank score using evidence from multiple features.
}
\usage{
  ensemble_rank(x,method=c('ProductOfRank','MeanOfLog','MeanOfLogLog'),small=1.0e-320)
}
\arguments{
 \item{x}{a matrix of discriminant values (e.g., P values) measuring the strength of the association between objects (eg modules) (in rows) and variables/features (in columns).}
 \item{method}{a character string specifyign the ranking metric. See \code{Details}.}
 \item{small}{offset the small p values before taking log transformation.}
}
\details{
Three ranking metrics are currently implemented:

ProductOfRank is the one used in Zhang et al. (Cell 2013, 153: 707-720) with a slight change to standardize the scores to be between 0 and 1; 

MeanOfLog computes the mean of -log(p value) which penalizes insignificant p values;

MeanOfLogLog computes the mean of log(-log(p value)) which penalizes insignificant p values and shrinks the difference in the ranges of p values.
}
\value{
 A vector of ranking scores between 0 to 1. The larger the score, the higher the rank.
}
\author{
Minghui Wang <m.h.wang@live.com>
}
\examples{
#Rank US cities by the significance test of excessive arrests compared to average
#compute the p value of one-tailed z-test
x=apply(USArrests,2,function(x) pnorm((x-mean(x))/sd(x),lower.tail=FALSE))
#compute ranking score using three different metrics
score1=ensemble_rank(x,method='ProductOfRank')
score2=ensemble_rank(x,method='MeanOfLog')
score3=ensemble_rank(x,method='MeanOfLogLog')
#plot the top 5 worst cities by each metric
par(mfrow=c(1,3))
barplot(sort(score1,decreasing=TRUE)[1:5],las=2)
barplot(sort(score2,decreasing=TRUE)[1:5],las=2)
barplot(sort(score3,decreasing=TRUE)[1:5],las=2)
}
