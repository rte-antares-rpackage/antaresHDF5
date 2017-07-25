library(rhdf5)
library(data.table)
library(h5)
library(antaresHdf5)
library(antaresRead)

path <- "testNewFormat3.h5"




setSimulationPath("E:/DES/antaresFlowbased/TestrunSimulationFb/antaresStudy")
#setSimulationPath("C:/Users/titorobe/Desktop/test_case")


system.time(W <- readAntares(areas = "fr", mcYears = 1:1000))

system.time(W <- readAntares(areas = "fr", mcYears = 1, select = "mustRun"))

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
microbenchmark::microbenchmark(W <- readAntares(clusters = "de", mcYears = 1:10, select = "production"),
                               V <- h5ReadAntares("testNewFormat3.h5",
                                                  clusters ="de", mcYears = 1:10, select = "production"))

Rprof(tmp <- tempfile())
W <- h5ReadAntares("testNewFormat3.h5",
              clusters ="de", mcYears = 1, select = "production")
Rprof()
summaryRprof(tmp)

res <- h5ReadAntares(path = path, areas = "all", mcYears = c(6, 7, 9:12), select = "all")




W <- readAntares(areas = "fr", mcYears = 1)


path
areas = "fr"
links = NULL
clusters = NULL
districts = NULL
mcYears = 1
timeStep = "hourly"
select = "all"
showProgress = TRUE
simplify = TRUE
perf = TRUE








path <- "bigStud.h5"
res <- h5ReadAntares(path = path,
                    areas = "all",
                    links = "all",
                    districts = "all",
                    clusters = "all",
                    mcYears = 1)

res <- h5ReadAntares(path = path,
                     areas = "all",
                     links = "all",
                     districts = "all",
                     clusters = "all",
                     mcYears = NULL)

res <- h5ReadAntares(path = path,
                     areas = "fr",
                     links = "be - de",
                     districts = "fr",
                     clusters = "fr",
                     mcYears = NULL)

res <- h5ReadAntares(path = path,
                     areas = "all",
                     links = "all",
                     districts = "all",
                     clusters = "all",
                     mcYears = "all")



res <- h5ReadAntares(path = path,
                     areas = "all",
                     links = "all",
                     districts = "all",
                     clusters = "all",
                     mcYears = "all",timeStep = "daily")

res <- h5ReadAntares(path = path,
                     areas = "all",
                     links = "all",
                     districts = "all",
                     clusters = "all",
                     mcYears = "all",timeStep = "weekly")

res <- h5ReadAntares(path = path,
                     areas = "all",
                     links = "all",
                     districts = "all",
                     clusters = "all",
                     mcYears = "all",timeStep = "monthly")

res <- h5ReadAntares(path = path,
                     areas = "all",
                     links = "all",
                     districts = "all",
                     clusters = "all",
                     mcYears = "all",timeStep = "annual")



path
areas = "all"
links = NULL
clusters = NULL
districts = NULL
mcYears = 1
timeStep = "hourly"
select = "all"
showProgress = TRUE
simplify = TRUE
perf = TRUE
