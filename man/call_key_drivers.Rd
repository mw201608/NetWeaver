  \name{call_key_drivers}
\alias{call_key_drivers}
\title{Identify Network Key Drivers for Signatures}
\description{
Identifies candidate key drivers by testing the enrichment of one or more
gene signatures in network neighborhoods.
}
\usage{
call_key_drivers(net, signature.df = NULL, nLayerToTest = 3,
  nLayersToExpand = 0, bg.size = NULL, directed = TRUE,
  reduce.within.nlayer = 2, fdr = 0.05,
  p.correction.method = "BH", return.overlap = FALSE)
}
\arguments{
\item{net}{A two-column matrix or data frame of network edges.}
\item{signature.df}{Optional matrix or data frame whose first two columns
contain signature members and their group labels, respectively.}
\item{nLayerToTest}{Maximum network distance from each candidate driver for testing signature enrichment.}
\item{nLayersToExpand}{Number of network layers used to expand each signature before
testing. Nodes within this distance from input signature will be used as the testing signature.}
\item{bg.size}{Background size for enrichment testing. Defaults to the number
of nodes in \code{net}.}
\item{directed}{Whether the network is directed.}
\item{reduce.within.nlayer}{Neighborhood distance used to reduce locally
connected key drivers.}
\item{fdr}{Adjusted p-value cutoff for significant key drivers.}
\item{p.correction.method}{Method passed to \code{p.adjust}.}
\item{return.overlap}{Whether to include overlapping signature members in the
result.}
}
\value{A data frame of predicted key drivers and enrichment statistics, or
\code{NULL} when no significant key drivers are found.}
\examples{
net <- data.frame(from = c("A", "B", "C"), to = c("B", "C", "D"))
signatures <- data.frame(Var = c("C", "D"), Group = "signature1")
\dontrun{call_key_drivers(net, signatures)}
}
