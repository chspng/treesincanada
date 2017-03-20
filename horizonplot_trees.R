# For infovis assignment #2
# This script generates horizonplots of the tree concentration by age and region

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

cat("Horizonplot script finished!\n");

