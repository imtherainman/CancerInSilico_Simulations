library('CancerInSilico')
library(methods)

generateHeatMap <- function(list_of_cell_models, num_rows) {

    mat_of_cell_models <-  matrix(list_of_cell_models, nrow = num_rows, byrow = TRUE)
    num_cols <- 
    final_density_mat <- matrix(nrow = num_rows, ncols = dim(mat_of_cell_models)[2])




}



##########################################

# filenames <- c("./sims_output/output_CIS_Adist_30_grAtoB_75.rds")
rdsFiles <- list.files(path = "./sims_output", full.names = TRUE, recursive = TRUE)
rdsFiles <- matrix(rdsFiles, nrow = 11, byrow = TRUE)
final_density_mat <- matrix(nrow = 11, ncol = 9)

for (row in seq(1,11)) {
  for (col in seq(1,9)) {

    # get file name (need to rename files for correct order)
    rds_file <- rdsFiles[row,col]

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
  	final_density_mat[row,col] <- finalDensity
  }
}
rownames(final_density_mat) <- sapply(strsplit(rdsFiles[,1],split="_"),function(x){paste(x[4],x[5])})
colnames(final_density_mat) <- sapply(strsplit(rdsFiles[1,],split="_"),function(x){paste(x[6],x[7])})
Heatmap(final_density_mat[paste('Adist',seq(from=0,to=100,by=10)),paste('grAtoB ',seq(from=0,to=200,by=25),'.rds', sep="")],cluster_columns = F,cluster_rows = F)
# Heatmap(matrix=final_density_mat)
