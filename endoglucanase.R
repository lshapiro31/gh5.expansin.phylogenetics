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

raxml_file <- system.file("gh5.RAxML_bipartitions.result") ## Need to figure out how to get the bootstrap values into the tree!
bootstrap <- read.raxml(raxml_file)

nwk <- ("gh5.rename.nwk")
tree <- read.tree(nwk)
str(tree)

p <- ggtree(tree)
p <- p %<+% annot
str(p)
dim(p)

tree$node.label # Obtain internal node labels
tree$tip.label # Obtain internal node labels

# Display internal node numbers
ggtree(tree) + geom_text2(aes(subset=!isTip, label=node), hjust=-.3) + geom_tiplab(size=3, color="black")

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
  geom_nodelab() +
  theme_tree() + 
  geom_treescale() +
  geom_tiplab(geom = "text", size=2.3, color="black") 
p + ggtitle("Glycoside Hydrolase 5 Phylogeny") +
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) +
  geom_cladelabel(node=90, label="Nematodes", align=TRUE, offset = 0.25) +
  geom_cladelabel(node=142, label="Enterobacteriaceae", align=TRUE) +
  geom_cladelabel(node=116, label="Enterobacteriaceae", align=TRUE) +
  geom_cladelabel(node=121, label="Enterobacteriaceae", align=TRUE) 




