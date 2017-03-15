# For infovis assignment #2
# This script generates heatmaps that display the number of trees by genus, age, and terrestrial ecozone
library(RColorBrewer)
library(ggplot2)
library("scales")

# Load & format dataset
alltrees <- read.csv("~/Documents/BMC/MSC\ Infovis/Assign2-trees/data/trees_by_ecozone_and_age.csv", sep="\t");
alltrees <- alltrees[,!colnames(alltrees) %in% c("Other", "Total")];
alltrees <- alltrees[!alltrees$Species.group %in% c("Unclassified","Other hardwoods", "Unspecified conifers", "Unspecified hardwoods", "Subtotal", "Total"),];
alltrees <- alltrees[!alltrees$Terrestrial.ecozone %in% c("Canada"),];

# Subset data
ecozones <- unique(alltrees$Terrestrial.ecozone);
treetypes <- unique(alltrees$Species.group);

# combo <- alltrees[, c(-2)];

# # edit row names so that they don't repeat
# rownames(combo) <- make.names(names = combo[,1],unique = TRUE)
# combo <- as.matrix(sapply(combo[,-1], as.numeric));

# save current directory
dir <- getwd();
cat("Saved the directory \n");

# output files
setwd("~/Documents/BMC/MSC\ Infovis/Assign2-trees/R\ output/");
cat("Changed the directory \n");

# OUTPUT: single plot
# pdf(file=paste (Sys.time(), "trees_in_canada_heatmaps.pdf", sep="-"));
# heatmap(x = combo, Rowv = NA, Colv = NA, scale="column", col=colorRampPalette(brewer.pal(9,"Greens"))(100));
# dev.off();

# OUTPUT: individual plots by tree type
# pdf(file=paste (Sys.time(), "individual_trees_in_canada_heatmaps.pdf", sep="-"));
# for (tree in treetypes){

# 	currentsubset <- alltrees[which(alltrees$Species.group == tree),];
# 	# Remove the terrestrial ecozone
# 	currentsubset <- currentsubset[,-2];
	
# 	# make a separate heatmap
# 	sub <- as.matrix(sapply(currentsubset[,-1], as.numeric));
# 	rownames(sub) <- currentsubset[,1]
	
# 	heatmap(x = sub, main = tree, Rowv = NA, Colv = NA, scale="column", col=colorRampPalette(brewer.pal(9,"Greens"))(100));
# }
# dev.off();

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

hm.palette <- colorRampPalette(brewer.pal(9, 'YlOrRd'), space='Lab')  



alltrees_reformatted$value1 <- cut(alltrees_reformatted$value, breaks = as.vector(quantile(alltrees_reformatted$value[alltrees_reformatted$value>0], na.rm=T)))

ggplot(alltrees_reformatted, 
	aes(x = ecozone, 
		y = tree_and_age)) + 
		geom_tile(aes(fill = value), colour = "white") + 
		# scale_fill_gradientn(colours = hm.palette(100)) + 
		scale_fill_gradientn(
			colours=c("white", "lightblue", "steelblue", "darkblue", "lightgreen", "green", "darkgreen"),
			values=rescale(c(0, 10, 500, 1000, 2000, 3000, 4000)),
			guide="colorbar") +
  # 		scale_fill_manual(values = c("white", "lightblue", "steelblue", "lightgreen", "green", "darkgreen"),
  #                      breaks = c("[0, 0.00001)", "[0.00001, 0.01)", "[0.01, 0.82)", "[0.82, 5.9)", "[5.9, 32.035)", "[32.035, 3858.640)")) +
		theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
	# scale_fill_gradient(low = "white", high = "steelblue", trans = "log") + 
	coord_flip() 



ggsave(paste (Sys.time(), "trees_in_canada_heatmaps.pdf", sep="-"))

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

