library(CancerInSilico)

# args <- commandArgs(TRUE)
# numOfRuns <- as.integer(args[1])
# arrayNum <- as.integer(args[2])
# jobName <- args[3]

numOfRuns <- 11

#### Set Parameters ####

initialNum <- 80
runTime <- 168
density <- 0.2
# cycleLengthDist <- function(type) {return(48)}
# cycleLengthDist <- replicate(numOfRuns, 20 + rexp(1000, 1/5))
drugEffect <- function(x) {return(1)}
# drugEffect <- c(function(x) {return(runif(1,0,1))})
inheritGrowth <- FALSE
outputIncrement <- 6
recordIncrement <- 0.25
randSeed <- 0
modelType <- "DrasdoHohme2003"
drugTime <- 0.0
boundary <- TRUE
nG <- 8
epsilon <- 10
syncCycles <- TRUE

typeA_dist <- seq(from = 0, to = 1, length.out = numOfRuns)
gr_AtoB_rat_list <- seq(from = 0, to = 2, length.out = 9)


########################

for (typeA_prop in typeA_dist) {
      for (gr_AtoB_rat in gr_AtoB_rat_list) {

            # Growth Rate Ratios, causing error

            cycleLengthDist_func <- function(type) {
                  if (type == 'A')
                  {
                        return (24)
                  }
                  else if (type == 'B')
                  {
                        return(24*gr_AtoB_rat)
                  }}

            output <- runCancerSim (initialNum=initialNum,
                        runTime=runTime,
                        density=density,
                        cycleLengthDist=cycleLengthDist_func,
                        inheritGrowth=inheritGrowth,
                        cellTypeInitFreq=c(typeA_prop, 1 - typeA_prop),
                        drugEffect=drugEffect,
                        outputIncrement=outputIncrement,
                        recordIncrement=recordIncrement,
                        randSeed=randSeed,
                        modelType=modelType,
                        drugTime=drugTime,
                        boundary=boundary,
                        syncCycles=syncCycles,
                        nG=nG,
                        epsilon=epsilon
                        )

            saveRDS(output, paste("output_CIS_Adist_", typeA_prop*100, "_grAtoB_",gr_AtoB_rat*100, ".rds", sep=""))
      }
}