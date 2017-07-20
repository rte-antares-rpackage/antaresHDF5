library(antaresRead)
library(antaresHdf5)

# opts <- setSimulationPath("D:/Users/titorobe/Desktop/test_case")
opts <- setSimulationPath("C:/Users/titorobe/Desktop/antaresStudy")

opts <-  setSimulationPath("E:/DES/antaresFlowbased/TestrunSimulationFb/antaresStudy")

for(i in 1:20)
{
W <- readAntares(areas = "all", links = "all", clusters = "all", districts = "all", mcYears = i)
}



H5close()
file.remove("testNewFormat3.h5")
#Rprof(tmp <- tempfile())


system.time(writeAntaresH5("testNewFormat3.h5", writeMcAll = TRUE, timeSteps = "hourly", compress = 1))
#summaryRprof(tmp)

areas <- c(as.character(unique(reZ$area)[1:8]))
select <- names(reZ[10:18])
system.time(reZ <- readAntares(areas = areas, mcYears = 1:15, select = select))
system.time(reZ <- readAntares(areas = "all", mcYears = "all"))

library(rhdf5)
library(plyr)
library(h5)
library(data.table)



system.time(K <- h5read("testNewFormat3.h5", "hourly/areas/mcInd/data"))
system.time(K1 <- arrayToDataTable(K))
K1

system.time(W <- readAntares(areas = "all", mcYears = "all"))






h5File <- h5file("testNewFormat3.h5")
K <- h5File["hourly/areas/mcInd/data"]

K
Struct <- data.table(h5ls("testNewFormat3.h5"))



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

