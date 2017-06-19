source("inst/dev/writeAttribFunction.R")
library(data.table)
library(antaresRead)
library(rhdf5)
setSimulationPath("D:/exemple_test", "SimulForH5")
res <- readAntares(areas = "all", links = "all", clusters = "all", districts = "all", mcYears = "all")



attrib <- attributes(res)
H5close()
file.remove("testWriteattrib.h5")
h5createFile("testWriteattrib.h5")
groupAttribs <- "attributes"
h5createGroup("testWriteattrib.h5", groupAttribs)
writeList(attrib, groupAttribs)

attribList <- h5dump(H5Gopen(H5Fopen("testWriteattrib.h5"), groupAttribs), all = T, read.attributes = TRUE)
attribList2 <- giveFormat(attribList)
class(attribList2$opts) <- "simOptions"
Errors <- testIdentical(attribList2,attrib )
Errors <- unlist(Errors)[!unlist(Errors)]
Errors





attribList2$opts$districtsDef





