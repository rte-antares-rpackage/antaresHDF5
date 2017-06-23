library(data.table)
library(antaresRead)
library(rhdf5)
library(pipeR)
library(stringr)
library(ggplot2)
library(antaresHdf5)




setSimulationPath("D:/Users/titorobe/Desktop/antaresStudy", 1)

path <- "bleble.h5"
#path <- "testWriteattrib.h5"
H5close()
file.remove(path)
h5createFile(path)
timeStepS <- c("hourly", "daily", "weekly",
               "monthly", "annual")

system.time(
  sapply(timeStepS, function(timeStep){
    print(timeStep)
    writeAntaresH5(path, timeStep)
  })
)

##57 sec pour tout le mcYears d'un coup
##Probleme dans le script dans la gestion de chunk, Temps normal : 19 sec pour 13 Mo


## 70 secondes pour 9 ans
## 424Mo -> 68Mo



organi <- data.table(h5ls(path))
h5C
fwrite(organi, "organi.csv", sep = ";")





output1 <- h5read(path, "hourly")


# library(parallel)
#
# cl <- makeCluster(4)
#
# res <- parLapplyLB(cl, 1:4, function(X){
#   library(rhdf5)
#
#   output <- h5dump(H5Gopen(H5Fopen("bleble.h5"), "hourly/data"), all = T, read.attributes = TRUE)
#   NULL
# })
# stopCluster(cl)

# system.time(output <- h5dump(H5Gopen(H5Fopen("bleble.h5"), "hourly/data")))

##Temps de lecture : Quand on coupe par mcYears 8 sec, sinon 4 sec.

system.time(rev <- readAntaresH5(path, select = "OV. COST"))#2.8

system.time(re2 <- readAntares(mcYears = "all", select = "OV. COST"))#4.3











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

