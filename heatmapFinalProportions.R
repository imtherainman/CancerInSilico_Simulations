library('CancerInSilico')
library('ComplexHeatmap')
library(methods)

getFinalProportionMat <- function(list_of_cell_models, num_rows) {

	final_proportion_data <- list()

	for (CellModelObj in list_of_cell_models) {

		# Params (heatmap axis values)
		init_proportion_typeA <- CellModelObj@cellTypeInitFreq[[1]]
		cycle_length_typeB <- CellModelObj@cellTypes[[2]]@minCycle

		# Get final proportion
		finalTime <- CellModelObj@runTime
		finalCellType_list <- getCellTypes(CellModelObj, finalTime)
		celltype_counts <- c(0,0)
		celltype_counts[1] <- sum(finalCellType_list == 1)
		celltype_counts[2] <- sum(finalCellType_list == 2)
		finalProportions <- celltype_counts[1]/sum(celltype_counts)

		# Add to list
		final_proportion_data[[length(final_proportion_data)+1]] <- c(init_proportion_typeA, cycle_length_typeB, finalProportions)
		
	}

	return(final_proportion_data)

}

# rownames(final_proportion_data) <- sapply(strsplit(rdsFiles[,1],split="_"),function(x){paste(x[4],x[5])})
# colnames(final_proportion_data) <- sapply(strsplit(rdsFiles[1,],split="_"),function(x){paste(x[6],x[7])})
# Heatmap(final_proportion_data[paste('Adist',seq(from=0,to=100,by=10)),paste('grAtoB ',seq(from=0,to=200,by=25),'.rds', sep="")],cluster_columns = F,cluster_rows = F)
# Heatmap(matrix=final_proportion_data)
