\name{getNeigobhors}
\alias{getNeigobhors}
\title{Find Neighbors of a Network Node}
\description{
Finds nodes reachable from a supplied node in one or more network layers.
}
\usage{
getNeigobhors(node, net, nLayer = 1, collapse = TRUE, directed = FALSE)
}
\arguments{
\item{node}{A node identifier.}
\item{net}{A two-column matrix or data frame of network edges.}
\item{nLayer}{Number of network layers to expand.}
\item{collapse}{Whether to return the final cumulative neighborhood rather
than a list of neighborhoods by layer.}
\item{directed}{Whether edges should be treated as directed.}
}
\value{A vector of neighboring node identifiers when \code{collapse} is
\code{TRUE}; otherwise, a list of node vectors by layer. Returns \code{NULL}
when no neighbors are found.}
\examples{
net <- data.frame(from = c("A", "B"), to = c("B", "C"))
getNeigobhors("A", net, nLayer = 1)
}
