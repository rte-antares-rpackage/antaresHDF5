context("h5ReadAntares")



alias <- showAliases()$name
alias <- as.character(alias)
##Data for test
zipPath <- system.file("testdata.zip", package = "antaresHdf5")
unzip(zipPath)
# setSimulationPath("testdata/test_case")
opts <- setSimulationPath("testdata/test_case")
writeAntaresH5(timeSteps = "annual")
path <- "20170315-1140eco-test.h5"
unlink(path, force = TRUE)

writeAntaresH5(misc = TRUE, thermalAvailabilities = TRUE,
               hydroStorage = TRUE, hydroStorageMaxPower = TRUE, reserve = TRUE,
               linkCapacity = TRUE,mustRun = TRUE, thermalModulation = TRUE, writeAllSimulations = TRUE)


timeStep <-  c("hourly", "daily", "weekly",
               "monthly", "annual")




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
##End data for test


# writeAntaresH5(writeAllSimulations = TRUE, nbCores = 2)
# testthat::expect_true(file.exists(paste0(list.files(paste0(opts$studyPath, "/output")), ".h5")))
# file.remove(paste0(list.files(paste0(opts$studyPath, "/output")), ".h5"))
#
#
#


sapply(pkgEnvH5$allCompute, function(X){
  test_that(paste0("Select : ", X, " timeStep : "),{
    param1 <- list(path = path, areas = "a", mcYears = 1, select = X)
    param2 <- list(path = path, areas = "a", mcYears = 1)
    param2[[X]] <- TRUE
    testthat::expect_true(identical(do.call(h5ReadAntares, param1),
                                    do.call(h5ReadAntares, param2)))
  })
})

##Test
paramComparaison <- list(
  areasAll = list(areas = "all"),
  linksAll = list(links = "all"),
  clustersAll = list(clusters = "all"),
  districtsAll = list(districts = "all"),
  areasAllMc1 = list(areas = "all", mcYears = 1),
  linksAllMc1 = list(links = "all", mcYears = 1),
  clustersAllMc1 = list(clusters = "all", mcYears = 1),
  districtsAllMc1 = list(districts = "all", mcYears = 1),
  areasAllMcAll = list(areas = "all", mcYears = "all"),
  linksAllMcAll = list(links = "all", mcYears = "all"),
  clustersAllMcAll = list(clusters = "all", mcYears = "all"),
  districtsAllMcAll = list(districts = "all", mcYears = "all"),
  areasaMcAll = list(area = "a", mcYears = "all"),
  linksBCMcAll = list(links = "b - c", mcYears = "all"),
  clustersaMcAll = list(clusters = "a", mcYears = "all"),
  districtsABMcAll = list(districts = "a and b", mcYears = "all"),
  linksFolowIn = list(links = "all", select = "FLOW LIN."),
  areasSelectAll = list(areas = "all", select = "all"),
  linksSelectAll = list(links = "all", select = "all"),
  clusterSelectAll = list(clusters = "all", select = "all"),
  districtsSelectAll = list(districts = "all", select = "all"),
  allData = list(areas = "all", links = "all", clusters = "all", districts = "all"),
  allDataMc1 = list(areas = "all", links = "all", clusters = "all", districts = "all", mcYears = 1),
  allDataMc2 = list(areas = "all", links = "all", clusters = "all", districts = "all", mcYears = 2),
  allDataMcAll = list(areas = "all", links = "all", clusters = "all", districts = "all", mcYears = "all")
)

#Test alias request
for(i in alias){
  paramComparaison[[i]] <- list(select = i)
}


#Test remove
for(i in alias){
  var <- strsplit(as.character(showAliases(i)$select[1]), ",")[[1]]
  for(j in var)
  {
  minus <- paste0("-", j)
  paramComparaison[[paste(i,minus)]] <- list(select = c(i, minus))
  }
}

sapply("hourly", function(Z){
  sapply(names(paramComparaison), function(X){
    test_that(paste(X, Z), {
      param1 <- paramComparaison[[X]]
      param1$timeStep <- Z
      param2 <- param1

      ##Silent
      param1$showProgress <- FALSE
      param2$perf <- FALSE

      ##End silent
      param2$path <- path
      DF1 <- do.call(readAntares, param1)
      DF2 <- do.call(h5ReadAntares, param2)
      if(!is(DF1, "antaresDataList"))
      {
        setorderv(DF1, getIdCols(DF1))
      }else{
        for(i in 1:length(DF1)){
          setorderv(DF1[[i]], getIdCols(DF1[[i]]))
        }
      }
      if(!is(DF2, "antaresDataList"))
      {
        setorderv(DF2, getIdCols(DF2))
      }else{
        for(i in 1:length(DF2)){
          setorderv(DF2[[i]], getIdCols(DF2[[i]]))
        }
      }
      expect_true(all(unlist(compareValue( DF1,DF2))))
    })
  })
})



test_that("processing", {
  
  optsH5 <- setSimulationPathH5(getwd(), path)
  addStraitments(opts = optsH5,  mcY = "mcInd",
                 addDownwardMargin = TRUE,
                 addUpwardMargin = TRUE,
                 addExportAndImport = TRUE,
                 addLoadFactorLink = TRUE,
                 externalDependency = TRUE,
                 loadFactor = TRUE,
                 modulation = TRUE,
                 netLoadRamp = TRUE,
                 surplus = TRUE,
                 surplusClusters = TRUE,
                 evalAreas = list(Tota = "`H. STOR` + `MISC. DTG`",
                                  Tota2 = "NODU + `NP COST` + 1"),
                 evalLinks = list(),
                 evalClusters = list(),
                 evalDistricts = list(),
                 columnsToSelects = c("H. STOR", " MISC. DTG", "NODU", "NP COST"))
})


test_that("h5ReadBindingConstraints", {
  optsH5 <- setSimulationPathH5(getwd(), path)
  re1 <- h5ReadBindingConstraints(optsH5)
  re2 <- antaresRead::readBindingConstraints(opts)
  for(i in 1:length(re1)){
    re1[[i]]$values <- data.frame(re1[[i]]$values )
    re2[[i]]$values <- data.frame(re2[[i]]$values )
    
  }
  expect_true(identical(re1, re2))
})
  
 
test_that("h5ReadLayout", {
  optsH5 <- setSimulationPathH5(getwd(), path)
  re1 <- h5ReadLayout(optsH5)
  re2 <- antaresRead::readLayout(opts)
  
  for(i in 1:length(re1)){
    re1[[i]] <- data.frame(re1[[i]])
    re2[[i]] <- data.frame(re2[[i]])
  }
  expect_true(identical(re1, re2))
})

test_that("h5ReadClusterDesc", {
  optsH5 <- setSimulationPathH5(getwd(), path)
  re1 <- data.frame(h5ReadClusterDesc(optsH5))
  re2 <- data.frame(antaresRead::readClusterDesc(opts))
  expect_true(identical(re1, re2))
})

 


H5close()
unlink(path, force = TRUE)
unlink("testdata", recursive = TRUE)

h5createFile("testnodata.h5")
h5createGroup("testnodata.h5", "hourly")
DF1 <-  h5ReadAntares("testnodata.h5", areas = "all", links = "all", clusters = "all", districts = "all")
H5close()
unlink("testnodata.h5")

