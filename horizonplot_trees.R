# For infovis assignment #2
# This script generates horizonplots of the tree concentration by age and region

library(RColorBrewer);
library(latticeExtra);

alltrees <- read.csv("~/Documents/BMC/MSC\ Infovis/Assign2-trees/data/trees_by_ecozone_and_age.csv", sep="\t");
alltrees <- alltrees[,!colnames(alltrees) %in% c("Other", "Total")];
alltrees <- alltrees[!alltrees$Species.group %in% c("Unclassified","Other hardwoods", "Unspecified conifers", "Unspecified hardwoods", "Subtotal", "Total"),];
alltrees <- alltrees[!alltrees$Terrestrial.ecozone %in% c("Canada"),];

# Subset data
ecozones <- unique(alltrees$Terrestrial.ecozone);
treetypes <- unique(alltrees$Species.group);

# Reformat data
tree_volume <- vector();

for (region in ecozones){
	
	for (tree in treetypes){

		row <- which(alltrees$Terrestrial.ecozone == region & alltrees$Species.group == tree, arr.ind = TRUE);
		
		if (length(row) == 1){
			
			tree_volume <- append(tree_volume, unlist(alltrees[row, c(-1,-2)], use.names = FALSE));	

			# treeage_vs_region[rowcount] <- append(treeage_vs_region, unlist(alltrees[row, c(-1,-2)], use.names = FALSE));	

		} else if (length(row) == 0) {
			tree_volume <- append(tree_volume, rep(NA, length(alltrees[,c(-1,-2)])));

			# treeage_vs_region[rowcount] <- append(treeage_vs_region, rep(NA, length(alltrees[,c(-1,-2)])));	

		} else {
			print("Error: returned more than one row")
		}		
	}
}

alltrees_reformatted <- data.frame(
	ecozone = rep(ecozones, each = (length(treetypes) * length(alltrees[,c(-1,-2)]))),
	tree = rep(rep(treetypes, each = length(alltrees[,c(-1,-2)])), length(ecozones)),
	age_range = rep(colnames(alltrees[,c(-1,-2)]), length(ecozones) * length(treetypes)),
	value = tree_volume
	);

alltrees_reformatted$tree_and_age <- paste(alltrees_reformatted$tree, alltrees_reformatted$age_range);

# output files
dir <- getwd();
setwd("~/Documents/BMC/MSC\ Infovis/Assign2-trees/R\ output/");

cols <- colorRampPalette(
	c("white", "white", "white", "white", "white", "white", "white", "white", 
	"lightgoldenrod1","#4fb258", "#0c734e", "#2c3666", "#292359", "#311d4c", "#3f0d3d", "#261118"));

trellis.device(
	device="pdf", 
	height = 10,
	width = 3,
	file=paste(Sys.time(), "trees_in_canada_horizonplot.pdf", sep="-"))

for (current_tree in treetypes){
	only_current_tree <- alltrees_reformatted[which(alltrees_reformatted$tree == current_tree), ]
	only_current_tree_m <- matrix(only_current_tree$value, ncol = length(ecozones))
	colnames(only_current_tree_m) <- ecozones;

	horizplot <- horizonplot(
		main = current_tree,
		x = ts(only_current_tree_m),
		nbands = 400L,
		origin = 0,
		horizonscale = 10,
		scales = list(y = list(relation = "same")),
		strip.left = FALSE,
	    ylab = list(rev(colnames(only_current_tree_m)), rot = 0, cex = 0.7),
	    col.regions = cols(800),    
	    colorkey = TRUE
		); 
	
	print(horizplot);
}

# As reference: 
# > quantile(alltrees_reformatted$value[alltrees_reformatted$value > 0], na.rm = T)
#       0%      25%      50%      75%     100% 
#    0.010    0.820    5.900   32.035 3858.640 
# > 

dev.off();
# I can remove the negative colour band in Illustrator

# ggsave(paste (Sys.time(), "trees_in_canada_heatmaps.pdf", sep="-"))

# return to previous directory
setwd(dir);
cat("Horizonplot script finished!\n");
