tpDir <- tempdir()
sourcedir <-  system.file("testdata", package = "antaresHdf5")

if (sourcedir != "") {
  if (Sys.info()['sysname'] == "Windows") {
    unzip(file.path(sourcedir, "testdata.zip"), exdir = tpDir)
  } else {
    unzip(file.path(sourcedir, "testdata.zip"), exdir = tpDir)
  }
  fil <- paste0(tpDir, "/testdata/test_case")
  
  opts <- antaresRead::setSimulationPath(fil)
  writeAntaresH5(path = tpDir, 
                 misc = TRUE, thermalAvailabilities = TRUE,
                 hydroStorage = TRUE, hydroStorageMaxPower = TRUE, reserve = TRUE,
                 linkCapacity = TRUE,mustRun = TRUE, thermalModulation = TRUE)
  path <- "20170315-1140eco-test.h5"
  h5fil <- path
  alias <- showAliases()$name
  alias <- as.character(alias)
  assign("tpDir", tpDir, envir = globalenv())
  assign("pathF", paste0(tpDir, "/", path), envir = globalenv())
  assign("h5fil", h5fil, envir = globalenv())
  assign("alias", alias, envir = globalenv())
}

