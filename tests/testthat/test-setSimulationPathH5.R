context("setSimulationPathH5")

test_that("identical setSimulationPathH5", {
  identical(setSimulationPathH5(tpDir), setSimulationPathH5(tpDir, 1))
})

