context("attrib write")


path <- system.file("testdata/testStudy.h5", package = "antaresHdf5")
fold <- gsub("/testStudy.h5", "", path)
fold <- paste0(fold, '/test_case')

setSimulationPath(fold, 1)
areas = "all"
links = "all"
districts = "all"
clusters = "all"
mcYears = c("all", "NO")
timeStepS = c("hourly", "daily", "weekly", "monthly" ,"annual")

testIdentical <- function(X, Y){
  sapply(names(X), function(Z){
    T1 <- X[[Z]]
    T2 <- Y[[Z]]
    if(class(T1)[1] %in% c("list", "simOptions", "data.table")){
      testIdentical(T1, T2)
    }else{
      if(class(T1)[1] == "factor" |  class(T2)[1] == "factor"){
        T1 <- as.character(T1)
        T2 <- as.character(T2)
        identical(T1, T2)
      }else{
        identical(T1, T2)
      }

    }
  })
}


TestSimili <- all(sapply(mcYears, function(mcYear)
{
  sapply(timeStepS, function(timeStep)
  {
    if(mcYear == "NO"){
      mcYear <- NULL
    }

    #timeStep <- timeStepS[1]
    out <- readAntaresH5(path = path, clusters = clusters, areas = areas, districts = districts,
                         links = links,timeStep = timeStep, mcYears = mcYear)


    VV <- readAntares( clusters = clusters, areas = areas, districts = districts,
                       links = links,timeStep = timeStep, mcYears = mcYear)
    Test <- NULL
    H <- attributes(out)
    V <- attributes(VV)
    Test <- c(Test, all(unlist(testIdentical(H, V))))
    Test <- c(Test, sapply(names(out), function(X){
      H <- attributes(out[[X]])
      V <- attributes(VV[[X]])
      all(unlist(testIdentical(H, V)))

    }))

    Test
  })
}))


expect_equal(all(TestSimili), TRUE)
