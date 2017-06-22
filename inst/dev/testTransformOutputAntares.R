setSimulationPath("D:/Users/titorobe/Desktop/test_case", 1)
res <- readAntares(areas = "all", links = "all", clusters = "all", districts = "all", mcYears = "all")


res2 <- transformH5(res)

res3 <- transformH5(res,areasKey = c("area", "mcYear"),
                    linksKey = c("link", "mcYear"),
                    districtKey = c("district", "mcYear"),
                    clustersKey = c("area", "cluster", "mcYear"))




H5close()
file.remove("testWrite22.h5")
h5createFile("testWrite22.h5")
h5createGroup("testWrite22.h5", "blo")
h5createGroup("testWrite22.h5", "bli")


system.time(writeAntaresData(res2, "testWrite22.h5", "blo"))

#23 secondes sans groupe / MCyears



Rprof(tmp <- tempfile())
system.time(writeAntaresData(res3, "testWrite22.h5", "bli"))#58 secondes sans groupe / MCyears
summaryRprof(tmp)



system.time(dt <- h5read("testWrite22.h5", "blo"))
system.time(dt2 <- h5read("testWrite22.h5", "bli"))


system.time(dt4 <- h5read("testWrite22.h5", "bli/areas/de"))


system.time(dt3 <- lapply(1:30, function(i){
  h5read("testWrite22.h5", paste0("/bli/areas/de/", i))
}))

Rprof(tmp <- tempfile())
writeAntaresData(res3, "testWrite22.h5", "bli")#61 secondes sans groupe / MCyears
summaryRprof(tmp)



h5ls("testWrite22.h5")



