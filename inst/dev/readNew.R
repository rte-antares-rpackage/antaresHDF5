library(rhdf5)
library(data.table)
library(h5)
library(antaresHdf5)
library(antaresRead)

path <- "testNewFormat3.h5"



setSimulationPath("E:/DES/antaresFlowbased/TestrunSimulationFb/antaresStudy")
system.time(W <- readAntares(areas = "all" ,mcYears = 1, simplify = TRUE,select = "NODU"))
W

Rprof(tmp <- tempfile())


system.time(V <- h5ReadAntares(path,
                               areas = c("all"),
                               mcYears = 1:10, select = "NODU"))



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
