context("Data read")


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


TestSimili <- sapply(timeStepS, function(timeStep){
  sapply(mcYears, function(mcYear)
  {
    if(mcYear == "NO"){
      mcYear <- NULL
    }
    out <- readAntaresH5(path = path, clusters = clusters, areas = areas, districts = districts,
                         links = links,timeStep = timeStep, mcYears = mcYear)


    VV <- readAntares( clusters = clusters, areas = areas, districts = districts,
                       links = links,timeStep = timeStep, mcYears = mcYear)

    identical(out$areas$time,VV$areas$time)
    ##Control identical
    all(unlist(sapply(names(out), function(y){
      sapply(names(out[[y]]), function(X){
        ifelse(identical(out[[y]][, .SD, .SDcols = X], VV[[y]][, .SD, .SDcols = X]),
               TRUE,
               if(class(unlist(out[[y]][, .SD, .SDcols = X])) %in% c("factor", "character") | class(unlist(VV[[y]][, .SD, .SDcols = X])) %in% c("factor", "character") )
               {identical(as.character(unlist(out[[y]][, .SD, .SDcols = X])),
                          as.character(unlist(VV[[y]][, .SD, .SDcols = X])))

               }else{
                 identical(as.numeric(unlist(out[[y]][, .SD, .SDcols = X])),
                           as.numeric(unlist(VV[[y]][, .SD, .SDcols = X])))
               })

      })

      # })
    })
    ))
  })
})

expect_equal(all(TestSimili), TRUE)
