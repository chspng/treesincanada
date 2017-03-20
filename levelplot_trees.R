# develop colour palette to maximize distinction

cols <- colorRampPalette(c("white","lightgoldenrod1","#4fb258", "#0c734e", "#2c3666", "#292359", "#311d4c", "#3f0d3d", "#261118"));

trellis.device(
	device="pdf", 
	height = 10,
	width = 5,
	file=paste(Sys.time(), "trees_in_canada_heatmap.pdf", sep="-"))

for (current_tree in treetypes){
	only_current_tree <- alltrees_reformatted[which(alltrees_reformatted$tree == current_tree), ]
	only_current_tree_m <- matrix(only_current_tree$value, ncol = length(ecozones))
	colnames(only_current_tree_m) <- ecozones;

	heatmap <- levelplot(		
		x = value ~ age_range * ecozone,
		data = only_current_tree,
		main = current_tree,
		colorkey = TRUE,
		col.regions = cols(800),
		region = TRUE,
		border = "white",
		border.lwd = 2,
		at = seq(0, 4000, 50),
		cuts = 20,
		scales = list(
			tck = 0
			)
		); 
	
	print(heatmap);
}

# As reference: 
# > quantile(alltrees_reformatted$value[alltrees_reformatted$value > 0], na.rm = T)
#       0%      25%      50%      75%     100% 
#    0.010    0.820    5.900   32.035 3858.640 
# > 

dev.off();

cat("Levelplot script finished\n");
