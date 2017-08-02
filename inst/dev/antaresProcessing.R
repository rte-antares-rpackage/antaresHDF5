library(antaresRead)

opts <- setSimulationPath("C:/Users/titorobe/Desktop/test_case", 1)
path <- "testStudy.h5"
writeAntaresH5(path, misc = TRUE, thermalAvailabilities = TRUE,
               hydroStorage = TRUE, hydroStorageMaxPower = TRUE, reserve = TRUE,
               linkCapacity = TRUE,mustRun = TRUE, thermalModulation = TRUE)


library(antaresProcessing)

dta <- h5ReadAntares("testStudy.h5", areas = "all", links = "all", clusters = "all", districts = "all",mcYears = "all")
antaresProcessing::addDownwardMargin(dta$areas)
antaresProcessing::addExportAndImport(dta$areas)
antaresProcessing::addLoadFactorLink(dta$links)
antaresProcessing::addNetLoad(dta$areas)
antaresProcessing::addUpwardMargin(dta$areas)
antaresProcessing::compare(dta$areas, dta$areas)
antaresProcessing::externalDependency(dta$areas)
antaresProcessing::getValues(dta$areas, "SOLAR")
antaresProcessing::loadFactor(dta$clusters)
antaresProcessing::modulation(dta$clusters)
antaresProcessing::netLoadRamp(dta$areas)
antaresProcessing::surplus(dta)
antaresProcessing::surplusClusters(dta)
antaresProcessing::synthesize(dta)
