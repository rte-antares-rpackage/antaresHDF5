

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
                                  Tota2 = "NODU + `NP COST` + 1"),
                 evalLinks = list(),
                 evalClusters = list(),
                 evalDistricts = list(),
                 columnsToSelects = c("H. STOR", " MISC. DTG", "NODU", "NP COST"))
})
