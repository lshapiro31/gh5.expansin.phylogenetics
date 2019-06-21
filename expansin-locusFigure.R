setwd("~Dropbox/R_Figures")

library(genoPlotR)
library(dplyr)

######################
### Expansin Locus ###
######################
getwd()
expansin <- read.delim('expansin.txt', header=FALSE, sep="\t")
expansin

## Dataframe with actual gene name labels
df.expansin <- data.frame(name = expansin[[2]], 
                          start = expansin[[3]], 
                          end = expansin[[4]], 
                          strand = expansin[[5]],
                          fill = expansin[[6]],
                          col="black")
df.expansin

dna_seg1 <- dna_seg(df.expansin)

dna_segs <- list(dna_seg1)

## Make sure the dna_seg object is what I think it is
is.dna_seg(dna_seg1)

## Get middles of the first dna_seg for annotations
mid <- middle(dna_segs[[1]])

plot_gene_map(dna_segs = dna_segs, 
              main = "Expansin-GH5 Operon",
              scale=TRUE)
