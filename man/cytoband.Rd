\name{ucsc.hg19.cytoband}
\docType{data}
\alias{ucsc.hg19.cytoband}
\alias{ucsc.hg38.cytoband}
\title{Human chromosome cytoband}
\description{
  Human chromosome cytoband information from UCSC  (http://genome.ucsc.edu/).
}
\usage{
data(ucsc.hg19.cytoband)
data(ucsc.hg38.cytoband)
}
\details{
Cytoband is useful for drawing the chromosome ideograms for the genome. Two human cytoband datasets are provided in this package, corresponding to two genome versions hg19 and hg38. Each cytoband dataset is a five-column tab-delimited data.frame. Each row of the data describes the position of a cytogenetic band.
}
\author{
Minghui Wang <m.h.wang@live.com>
}
\keyword{datasets}
\seealso{\code{\link{rc.initialize}}}
