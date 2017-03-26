# load packages
library(RColorBrewer);
library(latticeExtra);

# format data
source("~/code/treesincanada/format_data.R")

# generate plots
dir <- getwd();
setwd("~/Documents/BMC/MSC\ Infovis/Assign2-trees/R\ output/");

# source("~/code/treesincanada/horizonplot_trees.R")
source("~/code/treesincanada/levelplot_trees.R")
# source("~/code/treesincanada/data_preview.R")

setwd(dir);