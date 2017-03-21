# develop colour palette to maximize distinction

cols <- colorRampPalette(
	c("white","lightgoldenrod1","#4fb258", "#0c734e", "#2c3666", "#292359", "#311d4c", "#3f0d3d", "#261118"));

palette <- c("white", 
	"#fac0c1", "#fad0a9", "#fbdf8b", "#fdf063",
	"#e3e76e", "#d0e068", "#bdd862", "#a9d15d", "#95ca58", "#81c453", "#6abe50", "#4fb84c", "#24b245", 
	"#14a85b", "#0ea370", "#069e82", "#009992",
	"#15828d", "#1c758b", "#216989", "#255e86", "#295283", "#2c4580", 
	"#344774", "#3f4370", "#473e6d", "#4f3969", "#563466", "#5c2d63", "#61275f", "#661f5c",
	"#622255", "#5f224a", "#5b2240", "#572036", "#521e2b", "#4c1c21");

palette2 <- c("white", 
	"#fdf8c8", "#f7f4ad", "#f2ef92", "#edeb76",
	"#e3e76e", "#dde56f", "#d6e26f", "#cfe06f", "#c8dd6f", "#c1db70", "#bad870", "#b4d670", "#acd370", 
	"#8ec970", "#7bc579", "#67c081", "#4dbc88",
	"#48b297", "#4ca6a1", "#4f9aa9", "#528dae", "#5580b2", "#5673b8", 
	"#5764ae", "#6262a7", "#6c60a2", "#755e9d", "#7d5b99", "#855894", "#8d568f", "#94538b",
	"#884677", "#7f4068", "#763959", "#6c324a", "#612b3c", "#57242e", "#4c1c21");

palette3 <- c("white", 
	"#fdf8c8", "#f8f4af", "#f4f195", "#f1ed7c",
	"#ebeb78", "#e9ea7b", "#e7ea7f", "#e5e982", "#e3e884", "#e1e887", "#dfe78a", "#dde78d", "#dae68f", 
	"#d0e39a", "#c6e0a6", "#bbdeb2", "#b0dbbe",
	"#aeddd6", "#a3ccd3", "#98bdd1", "#8eafcf", "#84a2cd", "#7b94cc", 
	"#7b88c3", "#8987c1", "#9686bf", "#a285bd", "#ae84bb", "#ba83b9", "#c681b7", "#d180b5",
	"#d273ae", "#b7618e", "#a05374", "#89445c", "#733647", "#5f2933", "#4c1c21");

palette4 <- colorRampPalette(
	c("white", "#fdf8c8", "#019973", "#013b4c" ));

palette5 <- colorRampPalette(
	c("white", "#fcf497", "#41bb94", "#002733" ), bias = 1);

palette6 <- colorRampPalette(
	c("#fdf3b4", "#44bb8a", "#1b144f" ));

palette7 <- colorRampPalette(
	c("#fdf3b4", "#ba4747", "#1b144f" ));

palette8 <- colorRampPalette(
	c("#fdf3b4", "#70bf54", "#1b144f" ));

trellis.device(
	device="pdf", 
	height = 10,
	width = 5,
	file=paste(Sys.time(), "trees_in_canada_heatmap.pdf", sep="-"))

for (current_tree in treetypes){
	only_current_tree <- alltrees_reformatted[which(alltrees_reformatted$tree == current_tree), ]
	# only_current_tree_m <- matrix(only_current_tree$value, ncol = length(sorted_ecozones))
	# colnames(only_current_tree_m) <- sorted_ecozones;

	heatmap <- levelplot(		
		x = value ~ age_range * ecozone,
		data = only_current_tree,
		main = current_tree,
		colorkey = TRUE,
		col.regions = palette6,
		region = TRUE,
		border = "white",
		border.lwd = 1,
		# at = seq(0, 4000, 50),
		at = c(0, 
			0.01, 0.25, 0.5, 0.75, 1,
			seq(1.1, 10, 1),
			seq(10.1, 50, 10),
			seq(50.1, 200, 50),
			seq(200.1, 1000, 100),
			seq(1000.1, 4000.1, 500)
			),
		cuts = 20,
		scales = list(
			tck = 0
			)
		); 
	
	print(heatmap);
}

# > quantile(alltrees_reformatted$value, na.rm = T)
#       0%      25%      50%      75%     100% 
#    0.000    0.000    1.015   13.945 3858.640 

# > quantile(alltrees_reformatted$value[alltrees_reformatted$value > 0], na.rm = T)
#       0%      25%      50%      75%     100% 
#    0.010    0.820    5.900   32.035 3858.640 


dev.off();

cat("Levelplot script finished\n");
