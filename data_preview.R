trellis.device(
	device="pdf", 
	height = 10,
	width = 10,
	file=paste(Sys.time(), "data_preview.pdf", sep="-"))


zoom_steps = c(4000, 1000, 200, 50, 10, 2);

for (zoom in zoom_steps){
	
	scatter <- xyplot(
		x = value ~ tree, 
		data = alltrees_reformatted[alltrees_reformatted$value < zoom,],
		scales = list(
			rot = 90
			)
		);

	print(scatter);
}

	bar1 <- barchart(
		x = value ~ tree | ecozone, 
		data = alltrees_reformatted,
		scales = list(
			rot = 90
			)
		);

	print(bar1);


	bar2 <- barchart(
		x = value ~ tree | ecozone * age_range, 
		data = alltrees_reformatted,
		scales = list(
			rot = 90
			)
		);

	print(bar2);

# > quantile(alltrees_reformatted$value, na.rm = T)
#       0%      25%      50%      75%     100% 
#    0.000    0.000    1.015   13.945 3858.640 

# > quantile(alltrees_reformatted$value[alltrees_reformatted$value > 0], na.rm = T)
#       0%      25%      50%      75%     100% 
#    0.010    0.820    5.900   32.035 3858.640 


dev.off();
