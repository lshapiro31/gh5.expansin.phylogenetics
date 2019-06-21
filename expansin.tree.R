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

display.brewer.all()
colors_vec <- brewer.pal(7, name = 'Accent')
print(colors_vec)

setwd("~/Dropbox/AncientDNA/Expansin-endoglucanase-phylogeny/")
annot <- read_delim("expansin.annotations.R.txt", delim="\t")
str(annot)  # Read in as a tibble
names(annot)
select(annot, NCBI.Genus.species, label, Kingdom, Phylum, Class, Group, Color.Clade)  ## Reorder columns

# sumtrees.py -F newick RAxML_bootstrap.result > expansin.9.tre
## Then further edited in text editor:
## bootstraps from decimal to 0-100 scale
nwk <- ("expansin.9.tre") ## raxml output from sumtrees.py

tree_exp <- read.tree(nwk)
str(tree_exp)

tree_exp$node.label # Obtain internal node labels
tree_exp$tip.label # Obtain tip labels

# Display internal node numbers to select which clades to annotate
ggtree(tree_exp) + geom_text2(aes(subset=!isTip, label=node), hjust=-.3) + geom_tiplab(size=3, color="black")

## Attach annotations
tr_exp <- ggtree(tree_exp)
tr_exp_2 <- tr_exp %<+% annot
str(tr_exp_2)

## Specify color is by taxonomy from file
group_tree_exp <- split(tr_exp_2$data$label, tr_exp_2$data$Group)

str(group_tree_exp)
group_tree_exp <- groupOTU(tree_exp, group_tree_exp, group_name = "Taxonomic_Groups")
str(group_tree_exp)

#define colors for taxonomy coloring
colors1<-c("#a6cee3", "#1f78b4", "#b15928", "#b2df8a", "#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0", "#F0027F", "#BF5B17")

exp.plot <- ggplot(group_tree_exp, aes(color = Taxonomic_Groups)) + 
  geom_tree(size = 1.5) + 
  geom_nodelab(aes(label=label), color = "black", size=3, hjust = -0.3) + # This is the correct call
  theme_tree() + 
  geom_treescale(width = 0.5, linesize = 1, fontsize = 5, y = -5, x = 0) +
  geom_tiplab(aes(label=paste0('italic(', tr_exp_2$data$NCBI.Genus.species, ')')), size=3.5, color="black", parse=TRUE, hjust = -.1) +
  scale_color_manual(values = colors1) +
 # scale_color_manual(values = tr_exp_2$data$Color.Clade) +
  #scale_size_manual(values=c(1, 3)) +
  xlim(0,4)
exp.plot + ggtitle("Expansin Phylogeny") +
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5, size=16),
        legend.text = element_text(size = 9, face = "bold"),
        legend.title = element_text(size = 10, face = "bold"),
        legend.position = c(.2, .8))  +
  geom_cladelabel(node=88, label="Enterobacteriaceae", align=F, barsize = 1)


ggsave("expansin.tree.pdf", height = 8, width = 6)
dev.off()  
