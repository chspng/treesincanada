# For infovis assignment #2
# This script generates heatmaps that display the number of trees by genus, age, and terrestrial ecozone
library(RColorBrewer)

# Load & format dataset

alltrees <- read.csv("trees_by_ecozone_and_age.csv", sep="\t");

# I'm not going to bother with formatting the date ranges
# These are going to be re-done in Illustrator

# Remove "other" ages 
# Remove other tree types
# Plot totals separately as barcharts
alltrees <- alltrees[,!colnames(alltrees) %in% c("Other", "Total")];
alltrees <- alltrees[!alltrees$Species.group %in% c("Unclassified","Other hardwoods", "Unspecified conifers", "Unspecified hardwoods", "Subtotal", "Total"),];

# Subset data
# by each terrestrial ecozone
# by each tree genus
ecozones <- unique(alltrees$Terrestrial.ecozone);
treetypes <- unique(alltrees$Species.group);

for (tree in treetypes){

	currentsubset <- alltrees[which(alltrees$Species.group == tree),];
	# Remove the terrestrial ecozone
	currentsubset <- currentsubset[,-2];
	
	# make a separate heatmap
	for (i in nrow(currentsubset)){

		sub <- as.matrix(sapply(currentsubset[,-1], as.numeric));
		rownames(sub) <- currentsubset[,1]
		heatmap(x = sub, main = tree, Rowv = NA, Colv = NA, scale="column");

		# d3heatmap(sub, scale = "column", dendrogram = "none", color = "Blues")

	}
}

# for zones that don't have particular tree types, make it grey
# Ignore certain tree type (ex. "other")

# Generate heatmap
# Select colours
# Set breakpoints for binning
# Set meaningful titles (for later compositing)

# Output/save heatmaps