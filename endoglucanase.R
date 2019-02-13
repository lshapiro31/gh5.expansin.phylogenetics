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
names(annot)

nwk <- ("gh5.rename.nwk")
tree <- read.tree(nwk)

p <- ggtree(tree)
p <- p %<+% annot
str(tree)

# Display internal node numbers
ggtree(tree) + geom_text2(aes(subset=!isTip, label=node), hjust=-.3) + geom_tiplab(size=3, color="black")

#define colors for taxonomy coloring
colors1<-c("#a6cee3", "#1f78b4", "#b15928", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#cccc3d")
colors2<-c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#FFFF4D", "#b15928")

cls <- list(c1=(.node=c(129)), # Firmicutes
            c1=(.node=c(140)), # Firmicutes
            c2=(.node=c(125)), # Myxobacteria
            c2=(.node=c(142)), # Myxobacteria
            c3=(.node=c(147)), # Bacteroidetes
            c4=(.node=c(90)), # Worms
            c7=(.node=c(104)), # Alteromonadales
            c6=(.node=c(112)), # Vibrionales
            c6=(.node=c(106)), # Vibrionales
            c6=(.node=c(107)), # Vibrionales
            c5=(.node=c(116)), # Enterobacteria
            c5=(.node=c(121)), # Enterobacteria
            c5=(.node=c(142))) # Enterobacteria


tree <- groupClade(tree, cls)

p <- ggplot(tree, aes(color=group)) + 
  geom_tree() + 
  theme_tree() + 
  geom_treescale() +
  geom_tiplab(size=2, color="black") 
p + 
#  geom_cladelabel(node=129, label="Bacillales", align=TRUE, offset=.001) +
#  geom_cladelabel(node=140, label="Bacillales", align=TRUE) +
#  geom_cladelabel(node=125, label="Myxobacteria", align=TRUE) +
#  geom_cladelabel(node=147, label="Bacteroidales", align=TRUE) +
#  geom_cladelabel(node=112, label="Vibrionales", align=TRUE) +
#  geom_cladelabel(node=106, label="Vibrionales", align=TRUE) +
#  geom_cladelabel(node=108, label="Vibrionales", align=TRUE) +
#  geom_cladelabel(node=90, label="Nematodes", align=TRUE) +
  geom_cladelabel(node=142, label="Enterobacteriales", align=TRUE) +
  geom_cladelabel(node=116, label="Enterobacteriales", align=TRUE) +
  geom_cladelabel(node=121, label="Enterobacteriales", align=TRUE) 




