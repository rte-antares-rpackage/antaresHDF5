library(antaresRead)
# opts <- setSimulationPath("D:/Users/titorobe/Desktop/test_case")
opts <- setSimulationPath("D:/Users/titorobe/Desktop/antaresStudy")



path <- "testNewFormat2.h5"
timeSteps = c("hourly", "daily", "weekly", "monthly", "annual")
writeMcAll = TRUE
compress = 0

H5close()
file.remove("testNewFormat3.h5")
writeAntaresH5("testNewFormat3.h5", writeMcAll = TRUE, timeSteps = "hourly")




reZ <- readAntares(areas = "all", mcYears = "all")


library(rhdf5)
library(plyr)
library(data.table)

K <- h5read("testNewFormat3.h5", "hourly/areas/mcInd/data")
K1 <- arrayToDataTable(K)
K2 <- as.matrix(K1)
rm(K1)

arrayToDataTable <- function(array, dim = 2)
{
  ecraseDim <- dim(array)[dim]
  dimS <- 1:length(dim(array))
  dimNot <- dimS[-dim]
  prodDim <- prod(dim(array)[dimNot])
  arrayL <- list()
   for(i in 1:dim(array)[2]){
     arrayL[[i]] <- unlist(array[,i,,])
  }
  setattr(arrayL, "names", paste("V", 1:ecraseDim, sep = ""))
  setattr(arrayL, "row.names", .set_row_names(prodDim))
  setattr(arrayL, "class", c("data.table", "data.frame"))
  alloc.col(arrayL)
}

