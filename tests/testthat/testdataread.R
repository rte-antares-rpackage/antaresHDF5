context("Transform to h5")

zipPath <- system.file("testdata.zip", package = "antaresHdf5")
unzip(zipPath)

setSimulationPath("testdata/test_case")
path <- "testStudy.h5"
writeAntaresH5(path, misc = TRUE, thermalAvailabilities = TRUE,
               hydroStorage = TRUE, hydroStorageMaxPower = TRUE, reserve = TRUE,
               linkCapacity = TRUE,mustRun = TRUE, thermalModulation = TRUE)

compareValue <- function(A, B, res = NULL){
  if(class(A)[3] == "list"){
    res <- c(res, sapply(c("areas", "links", "cluster", "districts"), function(x){
      if(!is.null(A[[x]]))
      {
        compareValue(A[[x]], B[[x]], res = res)}}))

  }else{
    res <- c(res,sapply(names(A), function(X){
      if(identical(A[[X]], B[[X]])){
        TRUE
      }else{
        if(identical(as.numeric(A[[X]]), as.numeric(B[[X]])))
        {TRUE}else{
          identical(as.character(A[[X]]), as.character(B[[X]]))
        }
      }
    }))
  }

}

DF1 <- h5ReadAntares(path, area = 'all', links = "all", clusters = "all", districts = "all")
DF2 <- readAntares(area = 'all', links = "all", clusters = "all", districts = "all",misc = TRUE, thermalAvailabilities = TRUE,
                   hydroStorage = TRUE, hydroStorageMaxPower = TRUE, reserve = TRUE,
                   linkCapacity = TRUE,mustRun = TRUE, thermalModulation = TRUE)
setorderv(DF2$areas, c("area", "time"))
setorderv(DF2$links, c("link", "time"))
setorderv(DF2$districts, c("district", "time"))
setorderv(DF2$clusters, c("area", "cluster", "time"))
allComp <- unlist(compareValue( DF1,DF2))
testthat::expect_true(all(allComp))

DF1 <-   h5ReadAntares(path, area = 'all', links = "all", clusters = "all", districts = "all", mcYears = "all")
DF2 <- readAntares(area = 'all', links = "all", clusters = "all", districts = "all", mcYears = "all",misc = TRUE, thermalAvailabilities = TRUE,
                   hydroStorage = TRUE, hydroStorageMaxPower = TRUE, reserve = TRUE,
                   linkCapacity = TRUE,mustRun = TRUE, thermalModulation = TRUE)
setorderv(DF2$areas, c("mcYear","area", "time"))
setorderv(DF2$links, c("mcYear","link", "time"))
setorderv(DF2$districts, c("mcYear", "district","time"))
setorderv(DF2$clusters, c("mcYear", "area", "cluster","time"))
allComp <- unlist(compareValue(DF1, DF2))
testthat::expect_true(all(allComp))


DF1 <-  h5ReadAntares(path, area = 'all', links = "all", clusters = "all", districts = "all", mcYears = 1)
DF2 <-   readAntares(area = 'all', links = "all", clusters = "all", districts = "all", mcYears = 1,misc = TRUE, thermalAvailabilities = TRUE,
                     hydroStorage = TRUE, hydroStorageMaxPower = TRUE, reserve = TRUE,
                     linkCapacity = TRUE,mustRun = TRUE, thermalModulation = TRUE)
setorderv(DF2$areas, c("mcYear","area", "time"))
setorderv(DF2$links, c("mcYear","link", "time"))
setorderv(DF2$districts, c("mcYear", "district","time"))
setorderv(DF2$clusters, c("mcYear", "area", "cluster","time"))
allComp <- unlist(compareValue(DF1, DF2))
testthat::expect_true(all(allComp))



DF1 <- h5ReadAntares(path, area = 'a', links = "b - c", clusters = "a", districts = "a and b")
DF2 <- h5ReadAntares(path, area = 'a', links = "b - c", clusters = "a", districts = "a and b")
setorderv(DF2$areas, c("area", "time"))
setorderv(DF2$links, c("link", "time"))
setorderv(DF2$districts, c("district", "time"))
setorderv(DF2$clusters, c("area", "cluster", "time"))
allComp <- unlist(compareValue(DF1,DF2))
testthat::expect_true(all(allComp))


DF1 <-  h5ReadAntares(path, area = 'a', links = "b - c", clusters = "a", districts = "a and b", mcYears = 2)
DF2 <-   readAntares(area = 'a', links = "b - c", clusters = "a", districts = "a and b", mcYears = 2,misc = TRUE, thermalAvailabilities = TRUE,
                     hydroStorage = TRUE, hydroStorageMaxPower = TRUE, reserve = TRUE,
                     linkCapacity = TRUE,mustRun = TRUE, thermalModulation = TRUE)
setorderv(DF2$areas, c("mcYear","area", "time"))
setorderv(DF2$links, c("mcYear","link", "time"))
setorderv(DF2$districts, c("mcYear", "district","time"))
setorderv(DF2$clusters, c("mcYear", "area", "cluster","time"))
allComp <- unlist(compareValue(DF1, DF2))
testthat::expect_true(all(allComp))


DF1 <-  h5ReadAntares(path, area = 'a', links = "b - c", clusters = "a", districts = "a and b", mcYears = 2, timeStep = "annual")
DF2 <-   readAntares(area = 'a', links = "b - c", clusters = "a", districts = "a and b", mcYears = 2,misc = TRUE, thermalAvailabilities = TRUE,
                     hydroStorage = TRUE, hydroStorageMaxPower = TRUE, reserve = TRUE,
                     linkCapacity = TRUE,mustRun = TRUE, thermalModulation = TRUE, timeStep = "annual")
setorderv(DF2$areas, c("mcYear","area", "time"))
setorderv(DF2$links, c("mcYear","link", "time"))
setorderv(DF2$districts, c("mcYear", "district","time"))
setorderv(DF2$clusters, c("mcYear", "area", "cluster","time"))
allComp <- unlist(compareValue(DF1, DF2))
testthat::expect_true(all(allComp))


DF1 <-  h5ReadAntares(path, area = 'a', links = "b - c", clusters = "a", districts = "a and b", mcYears = 2, timeStep = "daily")
DF2 <-   readAntares(area = 'a', links = "b - c", clusters = "a", districts = "a and b", mcYears = 2,misc = TRUE, thermalAvailabilities = TRUE,
                     hydroStorage = TRUE, hydroStorageMaxPower = TRUE, reserve = TRUE,
                     linkCapacity = TRUE,mustRun = TRUE, thermalModulation = TRUE, timeStep = "daily")
setorderv(DF2$areas, c("mcYear","area", "time"))
setorderv(DF2$links, c("mcYear","link", "time"))
setorderv(DF2$districts, c("mcYear", "district","time"))
setorderv(DF2$clusters, c("mcYear", "area", "cluster","time"))
allComp <- unlist(compareValue(DF1, DF2))
testthat::expect_true(all(allComp))


DF1 <-  h5ReadAntares(path, area = 'a', links = "b - c", clusters = "a", districts = "a and b", mcYears = 2, timeStep = "weekly")
DF2 <-   readAntares(area = 'a', links = "b - c", clusters = "a", districts = "a and b", mcYears = 2,misc = TRUE, thermalAvailabilities = TRUE,
                     hydroStorage = TRUE, hydroStorageMaxPower = TRUE, reserve = TRUE,
                     linkCapacity = TRUE,mustRun = TRUE, thermalModulation = TRUE, timeStep = "weekly")
setorderv(DF2$areas, c("mcYear","area", "time"))
setorderv(DF2$links, c("mcYear","link", "time"))
setorderv(DF2$districts, c("mcYear", "district","time"))
setorderv(DF2$clusters, c("mcYear", "area", "cluster","time"))
allComp <- unlist(compareValue(DF1, DF2))
testthat::expect_true(all(allComp))

DF1 <-  h5ReadAntares(path, area = 'a', links = "b - c", clusters = "a", districts = "a and b", mcYears = 2, timeStep = "monthly")
DF2 <-   readAntares(area = 'a', links = "b - c", clusters = "a", districts = "a and b", mcYears = 2,misc = TRUE, thermalAvailabilities = TRUE,
                     hydroStorage = TRUE, hydroStorageMaxPower = TRUE, reserve = TRUE,
                     linkCapacity = TRUE,mustRun = TRUE, thermalModulation = TRUE, timeStep = "monthly")
setorderv(DF2$areas, c("mcYear","area", "time"))
setorderv(DF2$links, c("mcYear","link", "time"))
setorderv(DF2$districts, c("mcYear", "district","time"))
setorderv(DF2$clusters, c("mcYear", "area", "cluster","time"))
allComp <- unlist(compareValue(DF1, DF2))
testthat::expect_true(all(allComp))


DF1 <-  h5ReadAntares(path, area = 'a', mcYears = 2, timeStep = "monthly")
DF2 <-   readAntares(area = 'a', mcYears = 2,misc = TRUE,
                     hydroStorage = TRUE, hydroStorageMaxPower = TRUE, reserve = TRUE,
                     mustRun = TRUE, timeStep = "monthly")
setorderv(DF2, c("mcYear","area", "time"))
allComp <- unlist(compareValue(DF1, DF2))
testthat::expect_true(all(allComp))


DF1 <-  h5ReadAntares(path, areas = 'a', mcYears = 2, timeStep = "monthly", select = "OV. COST")
DF2 <-   readAntares(areas = 'a', mcYears = 2, timeStep = "monthly", select = "OV. COST")
setorderv(DF2, c("mcYear","area", "time"))
allComp <- unlist(compareValue(DF1, DF2))
testthat::expect_true(all(allComp))

DF1 <-  h5ReadAntares(path, links = 'a - b', mcYears = 2, timeStep = "monthly", select = "FLOW LIN.")
DF2 <-   readAntares(links = 'a - b', mcYears = 2, timeStep = "monthly", select = "FLOW LIN.")
setorderv(DF2, c("mcYear","link", "time"))
allComp <- unlist(compareValue(DF1, DF2))
testthat::expect_true(all(allComp))



DF1 <- h5ReadAntares(path, area = 'all', links = "all", clusters = "all", districts = "all",misc = TRUE, thermalAvailabilities = TRUE,
                     hydroStorage = TRUE, hydroStorageMaxPower = TRUE, reserve = TRUE,
                     linkCapacity = TRUE,mustRun = TRUE, thermalModulation = TRUE)
DF2 <- readAntares(area = 'all', links = "all", clusters = "all", districts = "all",misc = TRUE, thermalAvailabilities = TRUE,
                   hydroStorage = TRUE, hydroStorageMaxPower = TRUE, reserve = TRUE,
                   linkCapacity = TRUE,mustRun = TRUE, thermalModulation = TRUE)
setorderv(DF2$areas, c("area", "time"))
setorderv(DF2$links, c("link", "time"))
setorderv(DF2$districts, c("district", "time"))
setorderv(DF2$clusters, c("area", "cluster", "time"))
allComp <- unlist(compareValue( DF1,DF2))
testthat::expect_true(all(allComp))

testthat::expect_true(length(h5ReadAntares(path, select = "all", misc = TRUE)) == 0)
testthat::expect_true(length(h5ReadAntares(path, select = list(a = 1), misc = TRUE)) == 0)


H5close()
unlink(path, force = TRUE)
unlink("testdata", recursive = TRUE)

h5createFile("testnodata.h5")
h5createGroup("testnodata.h5", "hourly")
DF1 <-  h5ReadAntares("testnodata.h5", areas = "all", links = "all", clusters = "all", districts = "all")
H5close()
unlink("testnodata.h5")
