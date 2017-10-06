context("read inputs")

test_that("h5ReadBindingConstraints", {
  optsH5 <- setSimulationPathH5(tpDir, h5fil)
  re1 <- h5ReadBindingConstraints(optsH5)
  re2 <- antaresRead::readBindingConstraints(opts)
  for(i in 1:length(re1)){
    re1[[i]]$values <- data.frame(re1[[i]]$values )
    re2[[i]]$values <- data.frame(re2[[i]]$values )
    
  }
  expect_true(identical(re1, re2))
})


test_that("h5ReadLayout", {
  optsH5 <- setSimulationPathH5(tpDir, h5fil)
  re1 <- h5ReadLayout(optsH5)
  re2 <- antaresRead::readLayout(opts)
  
  for(i in 1:length(re1)){
    re1[[i]] <- data.frame(re1[[i]])
    re2[[i]] <- data.frame(re2[[i]])
  }
  expect_true(identical(re1, re2))
})

test_that("h5ReadClusterDesc", {
  optsH5 <- setSimulationPathH5(tpDir, h5fil)
  re1 <- data.frame(h5ReadClusterDesc(optsH5))
  re2 <- data.frame(antaresRead::readClusterDesc(opts))
  expect_true(identical(re1, re2))
})

