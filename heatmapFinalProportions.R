library('CancerInSilico')
library('ComplexHeatmap')
library(methods)

getFinalProportionMat <- function(list_of_cell_models, num_rows) {

	mat_of_cell_models <-  matrix(list_of_cell_models, nrow = num_rows, byrow = TRUE)
	num_cols <- dim(mat_of_cell_models)[2]
	final_proportion_mat <- matrix(nrow = num_rows, ncols = num_cols)

	for (r in seq(1,num_rows)) {
		for (c in seq(1,num_cols)) {

			CellModelObj <- mat_of_cell_models[r,c]

			finalTime <- CellModelObj@runTime
			finalCellType_list <- getCellTypes(CellModelObj, finalTime)
			celltype_counts <- c(0,0)
			celltype_counts[1] <- sum(finalCellType_list == 1)
			celltype_counts[2] <- sum(finalCellType_list == 2)
			finalDensity <- celltype_counts[1]/sum(celltype_counts)
			final_proportion_mat[r,c] <- finalDensity
		}
	}

	return(final_proportion_mat)
	
}

# rownames(final_proportion_mat) <- sapply(strsplit(rdsFiles[,1],split="_"),function(x){paste(x[4],x[5])})
# colnames(final_proportion_mat) <- sapply(strsplit(rdsFiles[1,],split="_"),function(x){paste(x[6],x[7])})
# Heatmap(final_proportion_mat[paste('Adist',seq(from=0,to=100,by=10)),paste('grAtoB ',seq(from=0,to=200,by=25),'.rds', sep="")],cluster_columns = F,cluster_rows = F)
# Heatmap(matrix=final_proportion_mat)
