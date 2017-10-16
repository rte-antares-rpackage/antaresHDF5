context("set aliases")



test_that("set h5 alias", {
  .setAlliasH5()
  expect_true("Out_surplusClusters" %in% silentf()$name)
})