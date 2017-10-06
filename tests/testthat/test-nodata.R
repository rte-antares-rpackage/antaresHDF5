context("No data")



test_that("no data", {
  h5createFile("testnodata.h5")
  h5createGroup("testnodata.h5", "hourly")
  DF1 <-  h5ReadAntares("testnodata.h5", areas = "all", links = "all", clusters = "all", districts = "all")
  H5close()
  expect_true(length(DF1) == 0)
  unlink("testnodata.h5")
})