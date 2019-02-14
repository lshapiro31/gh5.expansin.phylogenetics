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

## To remove attached packages 
# lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)

setwd("/Users/lorishapiro/Dropbox/AncientDNA/Expansin-endoglucanase-phylogeny/")
annot <- read.csv("endoglucanase.gh5.annotations.R.txt", header=T, sep="\t")
str(annot)  # Read in as a dataframe
names(annot)

# create new dataset without missing data 
annot <- na.omit(annot)

# RAxML output processed with sumtrees.py
# sumtrees.py gh5-RAxML_bootstrap.result -d 2 -F newick > gh5.new.nwk

nwk <- ("gh5.new.nwk") ## raxml output from sumtrees.py
tree <- read.tree(nwk)
str(tree)

tree$node.label # Obtain internal node labels
tree$tip.label # Obtain internal node labels

## Attach annotations
tr <- ggtree(tree)
tr2 <- tr %<+% annot
str(tr2)


## Specify color is by taxonomy from file
group_tree <- split(tr2$data$label, tr2$data$Phylum)
str(group_tree)
group_tree <- groupOTU(tree, group_tree, group_name = "Taxonomic_Groups")
str(group_tree)


# Display internal node numbers
ggtree(tree) + geom_text2(aes(subset=!isTip, label=node), hjust=-.3) + geom_tiplab(size=3, color="black")

#define colors for taxonomy coloring
colors1<-c("#a6cee3", "#1f78b4", "#b15928", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#cccc3d")

p <- ggplot(group_tree, aes(color = Taxonomic_Groups)) + 
  geom_tree() + 
  geom_nodelab(aes(label = label), color = "black", size=2.3) +
  theme_tree() + 
  geom_treescale() +
  geom_tiplab(geom = "text", size=2.3, color="black") +
  scale_color_manual(values = colors1) +
  theme(legend.position = "right")+
  scale_size_manual(values=c(1, 3)) + 
  geom_tiplab(color = "black", size = 2.3)
p + ggtitle("Glycoside Hydrolase 5 Phylogeny") +
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) +
 geom_cladelabel(node=137, label="Enterobacteriaceae", align=TRUE) +
 geom_cladelabel(node=116, label="Enterobacteriaceae", align=TRUE) +
 geom_cladelabel(node=111, label="Enterobacteriaceae", align=TRUE) 
  
