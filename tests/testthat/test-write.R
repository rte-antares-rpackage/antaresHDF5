context("Write h5")
 
tpDir2 <- gsub("[\\]", "/", tpDir)
tptpDir <- paste0(tpDir2, "/tpDir")
dir.create(tptpDir)

test_that("write more than one studies mono thread", {
writeAntaresH5(path = tptpDir,
               timeSteps = "annual", writeAllSimulations = TRUE, nbCores = 1)

})
unlink(tptpDir, recursive = TRUE)
dir.create(tptpDir)

# test_that("write more than one studies multi thread", {
#   writeAntaresH5(path = tptpDir,
#                  timeSteps = "annual", nbCores = 2, writeAllSimulations = TRUE)
# })
# unlink(tptpDir, recursive = TRUE)
