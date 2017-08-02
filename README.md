## NetWeaver [![CRAN](http://www.r-pkg.org/badges/version/NetWeaver)](https://cran.r-project.org/package=NetWeaver) [![Downloads](http://cranlogs.r-pkg.org/badges/NetWeaver?color=brightgreen)](http://www.r-pkg.org/pkg/NetWeaver)

### Description
`NetWeaver` is motivated towards developing a simple and flexible pipeline for visualizing the complex features of enrichment and correlation of gene coexpression network modules. While circos style 2D track plot is one natural choice for such practice, existing packages are designed primarily for handling genome structure and intervals. They are either too complicated to use, requiring certain level of knowledge of scripting, or limited in applications to only genomic structure data. To address these issues, particularly extend beyond applications in genomic structure data, `NetWeaver` offers a lightweight implementation of circular track plot, providing simple and flexible R function utilities and pipelines to generate circular images for visualizing different types of structure/relationship data.

### Installation
`NetWeaver` is available from `CRAN` so the simplest way to install in `R` is by running `install.packages("NetWeaver")`. To install the latest development from here in `github`, run `devtools::install_github("mw201608/NetWeaver")` in `R`.

### Usage
There are two sample pipelines:

1. Analysis of a real dataset of gene coexpression network modules is illustrated in `vignette("netweaver")`.
2. Analysis of a hypothetical data is shown in the sample code of function `rc.initialize`.
