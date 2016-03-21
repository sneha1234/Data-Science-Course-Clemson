library(parallel)

# ==============================================
# Author: Alex Herzog
# Date: March 3, 2016
# Purpose: Lab 08, Demonstrate parallel computing in R
# ==============================================

# Log file
sink("parScript.log", split=TRUE)

# Function to compute 2^x in parallel
testParallel <- function(ncores) {
    cl <- makeCluster(ncores)
    ptm <- proc.time()

    parLapply(cl, 1:5000000, function(x) 2^x)

    run.time <- proc.time() - ptm
    stopCluster(cl)
    run.time
}

for (i in 1:3) {
    x <- testParallel(i)
    print(x)
}

