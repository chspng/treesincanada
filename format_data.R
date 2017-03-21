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
	age_range = rep( paste(LETTERS[1:11], colnames(alltrees[,c(-1,-2)])), length(ecozones) * length(treetypes)),
	value = tree_volume
	);


# Sort ecozone data
counts_by_zone <- c();
i <- 1;
for (zone in ecozones){
	elements <- alltrees_reformatted$value[which(alltrees_reformatted$ecozone == zone)];
	elements <- elements[!is.na(elements)]
	sum <- Reduce("+", elements);
	counts_by_zone <- append(counts_by_zone, sum)
	i <- i + 1;
}
alltrees_reformatted$counts <- rep(counts_by_zone, each = (length(treetypes) * length(alltrees[,c(-1,-2)])));

alltrees_reformatted_sorted <- alltrees_reformatted[order(alltrees_reformatted$counts),]

sorted_ecozones <- unique(alltrees_reformatted_sorted$ecozone);

alltrees_reformatted$ecozone <- factor(rep(ecozones, each = (length(treetypes) * length(alltrees[,c(-1,-2)]))), levels = sorted_ecozones);

alltrees_reformatted[alltrees_reformatted == 0] <- NA;


# i <- 1;
# for (zone in sorted_ecozones){
# 	levels(alltrees_reformatted$ecozone)[levels(alltrees_reformatted$ecozone) == zone] <- paste(LETTERS[i], zone);	
# 	i <- i + 1;
# }

# levels(alltrees_reformatted$ecozone) <- ordered(sorted_ecozones)

cat("finished formatting data \n");
