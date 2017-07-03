WW <- readAntaresH5("smallStud.h5", areas = "all", links = "all", clusters = "all", districts = "all", mcYears = "all")

##Les traitements antaresProcessing :


##Add surplus columns
surplusAreas <- antaresProcessing::surplus(WW, timeStep = "hourly")
WW$areas <- merge(WW$areas, surplusAreas[, .SD, .SDcols = c("area", "mcYear", "time",
                                                            "consumerSurplus", "producerSurplus", "rowBalanceSurplus",
                                                            "storageSurplus", "congestionFees", "globalSurplus")],
                  by = c("area", "mcYear", "time"))

surplusCluster <- antaresProcessing::surplusClusters(WW, timeStep = "hourly")
WW$clusters <- merge(WW$clusters, surplusCluster[, .SD, .SDcols = c("area", "cluster", "mcYear", "time",
                                                                    "variableCost", "fixedCost", "startupCost",
                                                                    "surplusPerUnit", "totalSurplus", "economicGradient")],
                     by = c("area", "cluster", "mcYear", "time"))




WW$areas



antaresProcessing::surplusClusters(WW)
antaresProcessing::surplusSectors(WW)
antaresProcessing::addNetLoad(WW)
antaresProcessing::netLoadRamp(WW)
antaresProcessing::margins(WW)
antaresProcessing::modulation(WW)


test1 <- runif(100000000)
test2 <- test1
tpNam <-  paste0("ZZZ", round(runif(1)*10000,0),".h5")
file.create(tpNam)
system.time(h5write(test1, tpNam, "Tp"))


library(parallel)
cl <- makeCluster(4)
clusterExport(cl, "test1")
clusterEvalQ(cl, {
  library(rhdf5)
})
system.time(parLapplyLB(cl, 1:4, function(X){
  tpNam <-  paste0("ZZZ", round(runif(1)*10000,0),".h5")
  file.create(tpNam)
  h5write(test1, tpNam, "Tp")
}))

