context("Write h5")
 
tpDir2 <- gsub("[\\]", "/", tpDir)
tptpDir <- paste0(tpDir2, "/tpDir")
dir.create(tptpDir)

test_that("write more than one studies mono thread", {
writeAntaresH5(path = tptpDir,
               timeSteps = "annual", writeAllSimulations = TRUE, nbCores = 1, opts = optsG)

})
unlink(tptpDir, recursive = TRUE)

test_that("Bad path", {
  expect_error( writeAntaresH5('badPath'), "Folder badPath not found.")
 
})


# dir.create(tptpDir)
# test_that("write more than one studies multi thread", {
#   writeAntaresH5(path = tptpDir,
#                  timeSteps = "annual", writeAllSimulations = TRUE, nbCores = 2, opts = optsG)
# })
# unlink(tptpDir, recursive = TRUE)
