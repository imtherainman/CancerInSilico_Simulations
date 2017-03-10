library(CancerInSilico)

filenames <- c("./sims_output/output_CIS_Adist_30_grAtoB_75.rds")


for (rds_file in filenames) {

	# read in cell model object
	CellModelObj <- readRDS(rds_file)

	# get final state
	finalRow <- tail(CellModelObj@cells, n=1)[[1]]

	# radii lengths
	radii_lengths <- finalRow[seq(from=3,to=length(finalRow),by=7)]

	# sanity check
	stopifnot(length(radii_lengths) == length(finalRow)/7)
	numOfCells <- length(radii_lengths)

	# vector to hold cell type counts
	celltype_counts <- c(0,0)

	# count number of each celltype
	for (i in c(1:numOfCells)) {

		if (radii_lengths[i] > 0) {
			celltype_counts[finalRow[i*7] - 64] <- celltype_counts[finalRow[i*7] - 64] + 1
		}

	}

	finalDensity <- celltype_counts[1]/sum(celltype_counts)

	# Store in data frame
  }