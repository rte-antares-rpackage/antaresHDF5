

test_that("processing", {
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
  
  calcData <- readAntares(areas = "all", mcYears = "all", select = c("H. STOR" , "MISC. DTG",
                                                                     "NODU" , "NP COST",
                                                                     "Tota", "Tota2"))
  
  
  calcData[,verif1 := `H. STOR` + `MISC. DTG`]
  calcData[,verif2 := `NODU` + `NP COST` + 1]
  expect_true(max(calcData$Tota-calcData$verif1) == 0)
  expect_true(max(calcData$Tota2-calcData$verif2) == 0)
  
  
  
  
})
