

test_that("processing, write results", {
  optsH5 <- setSimulationPathH5(tpDir, h5fil)
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
                                  Tota2 = "`NODU` + `NP COST` + 1"),
                 evalLinks = list(),
                 evalClusters = list(),
                 evalDistricts = list())
})

test_that("processing calc by user", {
  optsH5 <- setSimulationPathH5(tpDir, h5fil)
  calcData <- readAntares(areas = "all", mcYears = "all", select = c("H. STOR" , "MISC. DTG",
                                                                   "NODU" , "NP COST",
                                                                   "Tota", "Tota2"), opts = optsH5)
  
  calcData[,verif1 := `H. STOR` + `MISC. DTG`]
  calcData[,verif2 := `NODU` + `NP COST` + 1]
  expect_true(max(calcData$Tota-calcData$verif1) == 0)
  expect_true(max(calcData$Tota2-calcData$verif2) == 0)
  
})


test_that("processing calc by straitements", {

UpwardMargin_out <- readAntares(areas = "all", mcYears = "all",select = "Out_addUpwardMargin")

UpwardMargin_recalc <- readAntares(areas = "all", mcYears = "all",select = "upwardMargin")
addUpwardMargin(UpwardMargin_recalc)

expect_true(identical(UpwardMargin_out$interconnectedUpwardMargin, 
                      UpwardMargin_recalc$areas$interconnectedUpwardMargin))

expect_true(identical(UpwardMargin_out$isolatedUpwardMargin, 
                      UpwardMargin_recalc$areas$isolatedUpwardMargin))

})