\name{NetWeaver}
\docType{package}
\alias{NetWeaver}
\alias{NetWeaver-package}
\title{Graphic Presentation of Complex Genomic and Network Data Analysis}
\description{
\code{NetWeaver} is motivated towards developing a simple and flexible pipeline for visualizing the complex features of enrichment and correlation of gene coexpression network modules. While circos style 2D track plot is one natural choice for such practice, existing packages are designed primarily for handling genome structure and intervals. They are either too complicated to use, requiring certain level of knowledge of scripting, or limited in applications to only genomic structure data. To address these issues, particularly extend beyond applications in genomic structure data, \code{NetWeaver} offers a lightweight implementation of circular track plot, providing simple and flexible R function utilities and pipelines to generate circular images for visualizing different types of structure/relationship data.\cr\cr
The original version of this package was developed for Figure 7 of Wang et al (2016) \emph{Genome Medicine} 8:104, which illustrates more than 20 properties for 50 coexpression network modules with a circular track plot. Please try to cite the paper when you use results from this software in a publication:\cr\cr
Wang M, Roussos P, McKenzie A, Zhou X, Kajiwara Y, Brennand K, DeLuca GC, Crary JF, Casaccia P, Buxbaum J et al. 2016. Integrative Network Analysis of Nineteen Brain Regions Identifies Molecular Signatures and Networks Underlying Selective Regional Vulnerability to Alzheimer's Disease. \emph{Genome Medicine} 8: 104.
}
\details{
There are two sample pipelines available:
\enumerate{
\item
Analysis of a real dataset of gene coexpression network modules can be reached through \code{vignette("netweaver")}.\cr
\item
Analysis of a hypothetical data is shown in sample code of \code{\link{rc.initialize}}.
}
}
\author{
Minghui Wang <m.h.wang@live.com>, Bin Zhang <bin.zhang@mssm.edu>
}
\references{
Wang M, Roussos P, McKenzie A, Zhou X, Kajiwara Y, Brennand K, DeLuca GC, Crary JF, Casaccia P, Buxbaum J et al. 2016. Integrative Network Analysis of Nineteen Brain Regions Identifies Molecular Signatures and Networks Underlying Selective Regional Vulnerability to Alzheimer's Disease. \emph{Genome Medicine} 8: 104.
}
\seealso{\code{\link{rc.initialize}}, \code{\link{Modules}}}
