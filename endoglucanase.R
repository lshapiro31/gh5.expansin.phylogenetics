library(treeio)
library(ggtree)
library(ape)
library(ggplot2)
library(tidytree)
library(colorspace)
library(purrr)
library(tidytree)
library(plyr)
library(dplyr)
library(magrittr)
library(readxl)
library(readr)

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
group_tree <- split(tr2$data$Class, tr2$data$Group)
str(group_tree)
group_tree <- groupOTU(tree, group_tree, group_name = "my_group")
str(group_tree)

trial<-ggtree(group_tree, aes(color = my_group)) #+ scale_color_manual(values = c("red", "black"))
trial

# Display internal node numbers
ggtree(tree) + geom_text2(aes(subset=!isTip, label=node), hjust=-.3) + geom_tiplab(size=3, color="black")

p <- ggplot(tree) + 
  geom_tree() + 
  geom_nodelab(aes(label = label), color = "black", size=2.3) +
#  geom_nodelab() +
  theme_tree() + 
  geom_treescale() +
  geom_tiplab(geom = "text", size=2.3, color="black") 
p + ggtitle("Glycoside Hydrolase 5 Phylogeny") +
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) +
  geom_cladelabel(node=90, label="Nematodes", align=TRUE, offset = 0.25) +
  geom_cladelabel(node=142, label="Enterobacteriaceae", align=TRUE) +
  geom_cladelabel(node=116, label="Enterobacteriaceae", align=TRUE) +
  geom_cladelabel(node=121, label="Enterobacteriaceae", align=TRUE) 




