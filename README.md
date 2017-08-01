# NetWeaver

## Description
NetWeaver is motivated towards developing a simple and flexible pipeline for visualizing the complex features of enrichment and correlation of gene coexpression network modules. While circos style 2D track plot is one natural choice for such practice, existing packages are designed primarily for handling genome structure and intervals. They are either too complicated to use, requiring certain level of knowledge of scripting, or limited in applications to only genomic structure data. To address these issues, particularly extend beyond applications in genomic structure data, NetWeaver offers a lightweight implementation of circular track plot, providing simple and flexible R function utilities and pipelines to generate circular images for visualizing different types of structure/relationship data.

## Usage
There are two sample pipelines:

1. Analysis of a real dataset of gene coexpression network modules can be reached through vignette("netweaver").
2. Analysis of a hypothetical data is shown in sample code of rc.initialize.
