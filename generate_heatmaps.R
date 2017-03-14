# For infovis assignment #2
# This script generates heatmaps that display the number of trees by genus, age, and terrestrial ecozone
library(RColorBrewer)
library(ggplot2)
# library(BoutrosLab.plotting.general)

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

# for each possible combination of tree type and age range and region
# check if the value exists
# if it does, add it to the vector
# if not, insert NA or 0

# initialize vectors

# make list of colnames for each tree and age per region
# tree_and_age <- vector();
# counter <- 1;

# for (region in ecozones){
# 	for (tree in treetypes){
# 		for (age in colnames(alltrees[,c(-1,-2)])){
# 			tree_and_age[counter] <- (paste(tree, age));
# 			counter <- counter + 1;
# 		}
# 	}
# }


# format counts of trees into list
# tree_count <- vector();

# There are 12 regions to iterate through: don't refer to them by levels() because this will include the regions I removed earlier
# for (region_counter in 1:length(ecozones)){
# 	for (tree_counter in 1:length(treetypes)){
# 		if (alltrees[region_counter,2] == treetypes[tree_counter]){
# 			# cat(alltrees[region_counter,tree_counter], treetypes[tree_counter], "\n", sep=" ");
# 			print("matches");
# 		} else {
# 			print("nope");

# 		}
# 	}
# }

# counters
# i <- 1; 
# current_ecozone <- 1;
# for (region in ecozones){

# 	current_treetype <- 1;
# 	for (tree in treetypes){

# 		for (age_row in dim(alltrees[,c(-1,-2)])[2]){
			
# 			# check if the tree is listed in the region
# 			if (alltrees$Terrestrial.ecozone[current_ecozone] == region){
				
# 				if (alltrees$Species.group[current_treetype] == tree){
# 					cat(paste(region, tree, alltrees[age_row + 2, current_treetype], "\n", sep = " "));

# 					# this part isn't working
# 					num_trees[i] <- alltrees[age_row + 2, current_treetype];
# 				} else {
# 					num_trees[i] <- NA;
# 				}
# 			}

# 			i <- i + 1;
# 		}
# 		current_treetype <- current_treetype + 1;
# 	}
# 	current_ecozone <- current_ecozone + 1;
# }


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

# treeage_vs_region <- matrix(
# 	tree_volume,
# 	nrow = length(ecozones),
# 	ncol = length(treetypes) * length(alltrees[,c(-1,-2)]),
# 	byrow = TRUE
# 	);


alltrees_reformatted <- data.frame(
	ecozone = rep(ecozones, each = (length(treetypes) * length(alltrees[,c(-1,-2)]))),
	tree = rep(rep(treetypes, each = length(alltrees[,c(-1,-2)])), length(ecozones)),
	age_range = rep(colnames(alltrees[,c(-1,-2)]), length(ecozones) * length(treetypes)),
	value = tree_volume
	);

alltrees_reformatted$tree_and_age <- paste(alltrees_reformatted$tree, alltrees_reformatted$age_range);


# plotting code
# create.heatmap(
# 	x = treeage_vs_region,
# 	clustering.method = 'none',

# 	)

# plot data



hm.palette <- colorRampPalette(brewer.pal(9, 'YlOrRd'), space='Lab')  

ggplot(alltrees_reformatted, 
	aes(x = ecozone, 
		y = tree_and_age,
		fill = value
		)) + geom_tile() + 
		scale_fill_gradientn(colours = hm.palette(100)) + 
		theme(axis.text.x = element_text(angle = 90, hjust = 1))

	# + scale_fill_gradient(low = "white", high = "steelblue")

# return to previous directory
setwd(dir);
cat("Returned to original directory \n");

cat("Script has run \n");

# combine all plots into one output
# OR set standard colour scale

# for zones that don't have particular tree types, make it grey

# Set breakpoints for binning

# Clean code

# Test code

