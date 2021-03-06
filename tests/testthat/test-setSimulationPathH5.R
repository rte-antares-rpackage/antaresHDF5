context("setSimulationPathH5")

test_that("identical setSimulationPathH5", {
  identical(setSimulationPathH5(tpDir), setSimulationPathH5(tpDir, 1))
})

test_that("Error no file", {
  expect_error(setSimulationPathH5("badfilename"),
               "Invalid path argument. File not found. Must be a .h5 file or a repertory with .h5 file(s)", fixed=TRUE)
})

test_that("Error no file", {
  empyfile <- paste0(tpDir, "\\nothing")
  dir.create(empyfile)
  expect_error(setSimulationPathH5(empyfile),
               "Not available .h5 file in your directory", fixed=TRUE)
})

