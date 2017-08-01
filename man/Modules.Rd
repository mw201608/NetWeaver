\name{Modules}
\docType{data}
\alias{Modules}
\title{Example Module Dataset}
\description{
  This example dataset and sample code illustrate the use of \code{NetWeaver} to visualize the complex features of gene coexpression network modules.
}
\usage{data(Modules)}
\details{
This dataset contains a data.frame called \code{Modules}. Each row is a module, with the module id in the first column. The second column is ranking score. The next 4 columns are coefficients of module-trait correlations. The rest columns are P values of enrichment for various gene signatures. The sample R code shows a pipeline for visualizing the module feature data using a circos style plot.
}
\author{
Minghui Wang <m.h.wang@live.com>, Bin Zhang <bin.zhang@mssm.edu>
}
\keyword{datasets}
\seealso{\code{\link{rc.initialize}}}
\examples{
\dontrun{
#see a tutorial for how to plot this dataset
vignette("netweaver")
}
}
