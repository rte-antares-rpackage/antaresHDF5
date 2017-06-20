library(data.table)
library(antaresRead)
library(rhdf5)
library(pipeR)
library(stringr)
setSimulationPath("D:/Users/titorobe/Desktop/test_case", 1)

path <- "bleble.h5"
#path <- "testWriteattrib.h5"
H5close()
file.remove(path)
h5createFile(path)
timeStepS <- c("hourly", "daily", "weekly", "monthly", "annual")
sapply(timeStepS, function(timeStep){
  print(timeStep)
  writeAntaresH5(path, timeStep)
})

organi <- data.table(h5ls(path))
fwrite(organi, "organi.csv", sep = ";")

writeAntaresH5 <- function(path, timeStep = "hourly", opts = antaresRead::simOptions()){

  res <- readAntares(areas = "all" ,
                     links = "all",
                     clusters = "all",
                     districts = "all",
                     mcYears = "all",
                     timeStep = timeStep, opts = opts)


  attrib <- attributes(res)
  # Create group
  h5createGroup(path, timeStep)

  #Write time
  writeTime(res, path, timeStep)

  #Write attributes
  writeAttribAndCreatGroup(path ,Y = attrib, timeStep)

  #Remove useless data
  sapply(1:length(res), function(i){
    res[[i]][, day := NULL]
    res[[i]][, month := NULL]
    res[[i]][, hour := NULL]
    res[[i]][, time := NULL]
  }) %>>% invisible()

  #Transform for write
  res <- transformH5(res,areasKey = c("area", "mcYear"),
                     linksKey = c("link", "mcYear"),
                     districtKey = c("district", "mcYear"),
                     clustersKey = c("area", "cluster", "mcYear"))

  writeAntaresData(res, path, timeStep)
}



output <- h5dump(H5Gopen(H5Fopen(path), "weekly"), all = T, read.attributes = TRUE)
struct <- h5readAttributes(path, "weekly/data/areas")$structure
tim <- getAllDateInfoFromDate(path, "weekly")
data.table(tim, cbindRecusive(output$data$areas, struct))


output <- h5dump(H5Gopen(H5Fopen(path), "hourly"), all = T, read.attributes = TRUE)
struct <- h5readAttributes(path, "hourly/data/links")$structure
tim <- getAllDateInfoFromDate(path, "hourly")
data.table(tim, cbindRecusive(output$data$links, struct))


output <- h5dump(H5Gopen(H5Fopen(path), "annual"), all = T, read.attributes = TRUE)
struct <- h5readAttributes(path, "annual/data/areas")$structure
tim <- getAllDateInfoFromDate(path, "annual")
data.table(tim, cbindRecusive(output$data$areas, struct))


output <- h5dump(H5Gopen(H5Fopen(path), "monthly"), all = T, read.attributes = TRUE)
struct <- h5readAttributes(path, "monthly/data/areas")$structure
tim <- getAllDateInfoFromDate(path, "monthly")
data.table(tim, cbindRecusive(output$data$areas, struct))

