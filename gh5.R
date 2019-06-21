library(treeio)
library(ggtree)
library(ape)
library(ggplot2)
library(colorspace)
library(purrr)
library(tidytree)
library(plyr)
library(dplyr)
library(magrittr)
library(readxl)
library(readr)
library(tidyverse)
library(RColorBrewer)
library(stringr)
library(stringi)
library(apTreeshape)

## To remove attached packages 
# lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)

setwd("~/Dropbox/AncientDNA/Expansin-endoglucanase-phylogeny/")
annot <- read_delim("gh5.annotations.R.txt", delim="\t")
str(annot)  # Read in as a tibble
names(annot)
select(annot, NCBI.Genus.species, label, Kingdom, Phylum, Class, Group, Color.Clade)  ## Reorder columns

nwk <- ("gh5.new.nwk2") ## raxml output from sumtrees.py

tree.gh5 <- read.tree(nwk)
str(tree.gh5)

tree.gh5$node.label # Obtain internal node labels
tree.gh5$tip.label # Obtain tip labels

# Display internal node numbers to select which clades to annotate
ggtree(tree.gh5) + geom_text2(aes(subset=!isTip, label=node), hjust=-.3) + geom_tiplab(size=3, color="black")

tree.gh5 <- di2multi(multid2di(tree.gh5))
tre.new <- rotate(tree.gh5, 93)

## Attach annotations
tr_gh5 <- ggtree(tree.gh5)
tr_gh5_2 <- tr_gh5 %<+% annot
str(tr_gh5_2)

## Specify color is by taxonomy from file
group_tree_gh5 <- split(tr_gh5_2$data$label, tr_gh5_2$data$Group)

str(group_tree_gh5)
group_tree_gh5 <- groupOTU(tree.gh5, group_tree_gh5, group_name = "Taxonomic_Groups")
str(group_tree_gh5)

## Visualize labels of internal nodes
plot(group_tree_gh5)
nodelabels()

#define colors for taxonomy coloring
colors1<-c("#b15928", "#a6cee3", "#1f78b4", "#b2df8a", "#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0", "#F0027F", "#BF5B17", "black", "grey")

gh5.plot <- ggplot(group_tree_gh5, aes(color = Taxonomic_Groups)) + 
  geom_tree(size = 1.5) + 
  geom_nodelab(aes(label=label), color = "black", size=3, hjust = -0.3 ) + 
  theme_tree() + 
  geom_treescale(width = 0.5, linesize = 1, fontsize = 5, y = -5, x = 0) +
#  geom_tiplab(size=3.5, color="black", parse=TRUE,hjust = -.1) +
  geom_tiplab(aes(label=paste0('italic(', tr_gh5_2$data$NCBI.Genus.species, ')')), size=3.5, color="black", parse=TRUE,hjust = -.1) +
  scale_color_manual(values = colors1) +
  xlim(0,4)
gh5.plot + ggtitle("Glycoside Hydrolase Family 5 Phylogeny") +
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5, size=16),
        legend.text = element_text(size = 9, face = "bold"),
        legend.title = element_text(size = 10, face = "bold"),
        legend.position = c(.2, .8)) +
 geom_cladelabel(node=137, label="Enterobacteriaceae", align=TRUE, barsize = 1) +
 geom_cladelabel(node=116, label="Enterobacteriaceae", align=TRUE, barsize = 1) +
 geom_cladelabel(node=111, label="Enterobacteriaceae", align=TRUE, barsize = 1) 

ggsave("gh5.tree.pdf", height = 11, width = 10)
dev.off()  


