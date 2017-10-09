context("Write h5")
 
tptpDir <- paste0(tpDir, "/tpDir")
dir.create(tptpDir)

test_that("write more than one studies mono thread", {
writeAntaresH5(path = tptpDir, 
               timeSteps = "annual", nbCores = 1)
  
})
unlink(tptpDir, recursive = TRUE)
dir.create(tptpDir)

test_that("write more than one studies multi thread", {
  writeAntaresH5(path = tptpDir, 
                 timeSteps = "annual", nbCores = 2)
})
unlink(tptpDir, recursive = TRUE)
