## NetWeaver [![CRAN](http://www.r-pkg.org/badges/version/NetWeaver)](https://cran.r-project.org/package=NetWeaver) [![Downloads](http://cranlogs.r-pkg.org/badges/NetWeaver?color=brightgreen)](http://www.r-pkg.org/pkg/NetWeaver)


### Trans-mQTLs in the developing human brain

Here I will demonstrate how to use `NetWeaver` to make a typical circos plot illustrating the genome-wide DNA methylation quantitative trait loci (mQTLs).
The mQTLs dataset that I am going to use was from Hannon et al (PMID 2661935) which studied the human fetal brain DNA methylation levels.
I downloaded the imputed set of mQTLs at link https://epigenetics.essex.ac.uk/mQTL/. There are 658492 SNP-methylation probe pairs in this dataset.
For simplicity, only the top 1000 most significant pairs will be used in this demonstration. You may load this preprocessed mQTLs data by:
```
library(NetWeaver)
load(mqtl)
head(mqtl)
```

A typical circos plot in the genomics study will generally show chromosome cytoband information in the out-most track. For convenience, two human chromosome cytobands for genome build hg19 and hg38 obtained from the UCSC genome browser have been included in the `NetWeaver` pakcage.
In this tutorial, we will use hg19 cytoband which can be imported by
```
data(ucsc.hg19.cytoband)

```
Note the hg38 version cytoband can be imported by `data(ucsc.hg38.cytoband)`.


Now, let us initialize the circos plot parameters and prepare the canvas:
```
rc.initialize(ucsc.hg19.cytoband, num.tracks=16, params=list(chr.padding=0.1))
rc.plot.area(size=0.9)
```

Plot ideogram and chromosome ids at track 1 and 2
```
rc.plot.ideogram(track.id=1:2, plot.band=TRUE, plot.chromosome.id=TRUE)
```


Prepare link data from mQTLs:
```
LinkData=data.frame(Chr1=paste0('chr',mqtl$SNP_Chr), Pos1=mqtl$SNP_BP, 
	Chr2=paste0('chr',mqtl$DNAm_CHR),Pos2=mqtl$DNAm_BP, Data=abs(mqtl$beta),stringsAsFactors=FALSE)
````

We will color the links by the chromosomal location of the mQTL SNPs.
```
chromosomes=paste0('chr',c(1:22,'X','Y'))
chr.factor2color=rlib:::labels2colors(chromosomes)
LinkData$Color=chr.factor2color[as.integer(factor(LinkData$Chr1,levels=chromosomes))]
```

Plot color bar for each chromosome.
```
chr.color=do.call(rbind,lapply(split(ucsc.hg19.cytoband,ucsc.hg19.cytoband$Chr),function(x) data.frame(Chr=x$Chr[1],Start=1,End=max(x$End),stringsAsFactors=FALSE)))
chr.color$Color=chr.factor2color[as.integer(factor(chr.color$Chr,levels=chromosomes))]
rc.plot.barchart(chr.color, track.id=1, color.col="Color", custom.track.height=rc.get.params()$track.height/3, track.border=NA)
```

Lastly, plot the links:
```
rc.plot.link(LinkData, track.id=4, data.col=5,color.col=6)
