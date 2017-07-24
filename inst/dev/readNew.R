library(rhdf5)
library(data.table)
library(h5)
library(antaresHdf5)
library(antaresRead)

path <- "testNewFormat3.h5"




#setSimulationPath("E:/DES/antaresFlowbased/TestrunSimulationFb/antaresStudy")
setSimulationPath("C:/Users/titorobe/Desktop/test_case")


system.time(W <- readAntares(areas = "fr", mcYears = 1:1000))
names(W)

library(pipeR)
writeAntaresH5("toto.h5", compress = 1,  misc = TRUE,
               thermalAvailabilities = TRUE,
               hydroStorage = TRUE, hydroStorageMaxPower = TRUE, reserve = TRUE,
               linkCapacity = TRUE, mustRun = TRUE, thermalModulation = TRUE)



path <- "testNewFormat3.h5"
#path <- "toto.h5"
V <- h5ReadAntares(path,
                   clusters ="all",
                   mcYears = 1:100)

Bli2 <- h5ReadAntares(path, areas = "all")
h5ls("toto.h5")



V <- h5ReadAntares("testNewFormat3.h5",
                   clusters ="at", mcYears = 1, select = "production")





path <- "testNewFormat3.h5"
areas = NULL
links = NULL
clusters = "at"
districts = NULL
mcYears = 1
timeStep = "hourly"
select = "production"
showProgress = TRUE
simplify = TRUE
perf = TRUE




S <- readAntares(cluster = "fr")

V
W




path
areas = "all"
links = NULL
clusters = NULL
districts = NULL
mcYears = 1:10
timeStep = "hourly"
select = "NODU"
showProgress = TRUE
simplify = TRUE
