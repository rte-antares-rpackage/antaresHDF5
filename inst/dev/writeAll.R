library(data.table)
library(antaresRead)
library(rhdf5)
library(pipeR)
library(stringr)
library(ggplot2)





setSimulationPath("D:/Users/titorobe/Desktop/antaresStudy", 1)

path <- "bleble.h5"
#path <- "testWriteattrib.h5"
H5close()
file.remove(path)
h5createFile(path)
timeStepS <- c("hourly")

Rprof(tmp <- tempfile())
sapply(timeStepS, function(timeStep){
  print(timeStep)
  writeAntaresH5(path, timeStep)
})
summaryRprof(tmp)
unlink(tmp)

##Probleme dans le script dans la gestion de chunk, Temps normal : 19 sec pour 13 Mo


## 70 secondes pour 9 ans
## 424Mo -> 68Mo



organi <- data.table(h5ls(path))
fwrite(organi, "organi.csv", sep = ";")

writeAntaresH5 <- function(path, timeStep = "hourly", opts = antaresRead::simOptions()){

  allMcYears <- opts$mcYears
  print(allMcYears)
  sapply(allMcYears, function(mcY)
  {

    if(allMcYears[1] == mcY){
      writeStructure = TRUE
    }else{
      writeStructure = FALSE
    }

    res <- readAntares(areas = "all" ,
                       links = "all",
                       clusters = "all",
                       districts = "all",
                       mcYears = mcY,
                       timeStep = timeStep, opts = opts)


    if(writeStructure){

      attrib <- attributes(res)
      # Create group
      h5createGroup(path, timeStep)

      #Write time
      writeTime(res, path, timeStep)

      #Write attributes
      writeAttribAndCreatGroup(path ,Y = attrib, timeStep)

    }


    #Remove useless data
    sapply(1:length(res), function(i){
      if("day" %in% names(res[[i]])){
        res[[i]][, day := NULL]
      }
      if("month" %in% names(res[[i]])){
        res[[i]][, month := NULL]
      }
      if("hour" %in% names(res[[i]])){
        res[[i]][, hour := NULL]
      }
      if("time" %in% names(res[[i]])){
        res[[i]][, time := NULL]
      }
    }) %>>% invisible()

    #Transform for write
    res <- transformH5(res,areasKey = c("area", "mcYear"),
                       linksKey = c("link", "mcYear"),
                       districtKey = c("district", "mcYear"),
                       clustersKey = c("area", "cluster", "mcYear"))

    writeAntaresData(res, path, timeStep, writeStructure)
  })

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

