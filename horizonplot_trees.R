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

spruce_test <- alltrees_reformatted[which(alltrees_reformatted$tree == 'Spruce'), ]
spruce_test_m <- matrix(spruce_test$value, ncol = length(ecozones))
colnames(spruce_test_m) <- ecozones;

cols <- colorRampPalette(c("white", "lightgoldenrod1", "darkgreen"));

horizplot <- horizonplot(
	x = ts(spruce_test_m),
	nbands = 160L,
	origin = 0,
	horizonscale = 25,
	scales = list(y = list(relation = "same")),
	strip.left = FALSE,
    ylab = list(rev(colnames(spruce_test_m)), rot = 0, cex = 0.7),
    # col.regions = grey.colors(60, start = 0.9, end = 0.3),	 
    col.regions = cols(320),
    colorkey = TRUE
	) 

trellis.device(device="pdf", file=paste(Sys.time(), "trees_in_canada_horizonplot.pdf", sep="-"))
print(horizplot)

dev.off();
# I can remove the negative colour band in Illustrator

# ggsave(paste (Sys.time(), "trees_in_canada_heatmaps.pdf", sep="-"))

# return to previous directory
setwd(dir);
cat("Horizonplot script finished!\n");