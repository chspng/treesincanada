# For infovis assignment #2
# This script generates heatmaps that display the number of trees by genus, age, and terrestrial ecozone
library(RColorBrewer)
library(ggplot2)

# Load & format dataset

alltrees <- read.csv("~/Documents/BMC/MSC\ Infovis/Assign2-trees/data/trees_by_ecozone_and_age.csv", sep="\t");

# I'm not going to bother with formatting the date ranges
# These are going to be re-done in Illustrator

# Remove "other" ages 
# Remove other tree types
# Plot totals separately as barcharts
alltrees <- alltrees[,!colnames(alltrees) %in% c("Other", "Total")];
alltrees <- alltrees[!alltrees$Species.group %in% c("Unclassified","Other hardwoods", "Unspecified conifers", "Unspecified hardwoods", "Subtotal", "Total"),];
alltrees <- alltrees[!alltrees$Terrestrial.ecozone %in% c("Canada"),];

# Subset data
# by each terrestrial ecozone
# by each tree genus
ecozones <- unique(alltrees$Terrestrial.ecozone);
treetypes <- unique(alltrees$Species.group);

combo <- alltrees[, c(-2)];

# edit row names so that they don't repeat
rownames(combo) <- make.names(names = combo[,1],unique = TRUE)
combo <- as.matrix(sapply(combo[,-1], as.numeric));

# this current heatmap is flipped - I want the age ranges to be repeated, and the ecozones to only appear once 
# i may need to reformat the table to get this

# save current directory


dir <- getwd();
cat("Saved the directory \n");

# output files
setwd("~/Documents/BMC/MSC\ Infovis/Assign2-trees/R\ output/");
cat("Changed the directory \n");

# OUTPUT: single plot
pdf(file=paste (Sys.time(), "trees_in_canada_heatmaps.pdf", sep="-"));
heatmap(x = combo, Rowv = NA, Colv = NA, scale="column", col=colorRampPalette(brewer.pal(9,"Greens"))(100));
dev.off();

# OUTPUT: individual plots by tree type
pdf(file=paste (Sys.time(), "individual_trees_in_canada_heatmaps.pdf", sep="-"));
for (tree in treetypes){

	currentsubset <- alltrees[which(alltrees$Species.group == tree),];
	# Remove the terrestrial ecozone
	currentsubset <- currentsubset[,-2];
	
	# make a separate heatmap
	sub <- as.matrix(sapply(currentsubset[,-1], as.numeric));
	rownames(sub) <- currentsubset[,1]
	
	heatmap(x = sub, main = tree, Rowv = NA, Colv = NA, scale="column", col=colorRampPalette(brewer.pal(9,"Greens"))(100));
}
dev.off();

# ggplot output
# reformat data to be in the form I want
# flatten region rows, combine tree types into age ranges
# take the first row of alltrees data frame
# make a list with combining the age classes with the tree type
# append this list to the new data frame
# continue with the second row, and tack onto the same row
# start a new row for every new ecozone

# for each tree type, and for each age range create a list of colnames
tree_and_age <- vector();
i <- 1; 
for (tree in treetypes){
	for (age in colnames(alltrees[,c(-1,-2)])){
		tree_and_age[i] <- (paste(tree, age));
		i <- i + 1;
	}
}

alltrees_reformatted <- data.frame(
	ecozones = ecozones,
	treetypeandage = tree_and_age,
	value = 
	);

ggplot(alltrees_reformatted, 
	aes(x = ecozones, 
		y = treetypeandage,
		fill = value)) 
	+ geom_tile() 
	+ scale_fill_gradient(low = "white", high = "steelblue")

# return to previous directory
setwd(dir);
cat("Returned to original directory \n");

cat("Script has run \n");

# combine all plots into one output
# OR set standard colour scale

# for zones that don't have particular tree types, make it grey

# Set breakpoints for binning

