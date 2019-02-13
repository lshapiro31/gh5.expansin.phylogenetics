library(treeio)
library(ggtree)
library(ape)
library(ggplot2)
library(tidytree)
library(colorspace)

## To remove attached packages 
lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)

setwd("/Users/lorishapiro/Dropbox/AncientDNA/Expansin-endoglucanase-phylogeny/")

nwk <- ("gh5.rename.nwk")
tree <- read.tree(nwk)

str(tree)

# Display internal node numbers
ggtree(tree) + geom_text2(aes(subset=!isTip, label=node), hjust=-.3) + geom_tiplab(size=2, color="black")

cls <- list(c1=(.node=c(129, 140)), # Firmicutes
            c2=(.node=c(142, 125)), #Myxobacteria
            c3=(.node=c(140, 156, 99))) # Enterobacteria

tree <- groupClade(tree, cls)

p <- ggplot(tree, aes(color=group)) + 
  geom_tree() + 
  theme_tree() + 
  geom_treescale() +
  geom_tiplab(size=2, color="black") 
p + geom_cladelabel(node=129, label="Bacillales", align=TRUE, offset=.001) +
  geom_cladelabel(node=140, label="Bacillales", align=TRUE) +
  geom_cladelabel(node=125, label="Myxobacteria", align=TRUE) +
  geom_cladelabel(node=152, label="Bacteroidales", align=TRUE) +
  geom_cladelabel(node=112, label="Vibrionales", align=TRUE) +
  geom_cladelabel(node=106, label="Vibrionales", align=TRUE) +
  geom_cladelabel(node=108, label="Vibrionales", align=TRUE) +
  geom_cladelabel(node=142, label="Enterobacteriales", align=TRUE) +
  geom_cladelabel(node=116, label="Enterobacteriales", align=TRUE) +
  geom_cladelabel(node=121, label="Enterobacteriales", align=TRUE) 



## One way to group clades
tree <- groupClade(tree, .node=c(139, 116))
ggtree(tree, aes(color="red", linetype=group))