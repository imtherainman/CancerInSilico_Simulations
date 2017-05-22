library('CancerInSilico')
library(methods)


readFilesAsList <- function(path_to_files) {

    # get all file names
    allFiles <- list.files(path=path_to_files, full.names = TRUE, recursive = TRUE)

    # initalize empty list
    CellModelObj_list <- vector("list", length = length(allFiles))

    # read in cell model objects
    ind <- 1
    for (file in allFiles) {

        CellModelObj_listp[[ind]] <- load(allFiles[[ind]])
        ind <- ind + 1
    }

    return(CellModelObj_list)

}