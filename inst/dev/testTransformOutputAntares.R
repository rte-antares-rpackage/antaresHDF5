setSimulationPath("D:/exemple_test", "SimulForH5")
res <- readAntares(areas = "all", links = "all", clusters = "all", districts = "all", mcYears = "all")


class(attributes(res)$opts)


res2 <- transformH5(res)

res3 <- transformH5(res,areasKey = c("area", "mcYear"),
                    linksKey = c("link", "mcYear"),
                    clustersKey = c("area", "cluster", "mcYear"))




H5close()
file.remove("testWrite22.h5")
h5createFile("testWrite22.h5")
h5createGroup("testWrite22.h5", "blo")
h5createGroup("testWrite22.h5", "bli")


system.time(writeAntaresH5(res2, "testWrite22.h5", "blo"))

#23 secondes sans groupe / MCyears



Rprof(tmp <- tempfile())
system.time(writeAntaresH5(res3, "testWrite22.h5", "bli"))#58 secondes sans groupe / MCyears
summaryRprof(tmp)



system.time(dt <- h5read("testWrite22.h5", "blo"))
system.time(dt2 <- h5read("testWrite22.h5", "bli"))


system.time(dt4 <- h5read("testWrite22.h5", "bli/areas/de"))


system.time(dt3 <- lapply(1:30, function(i){
  h5read("testWrite22.h5", paste0("/bli/areas/de/", i))
}))

Rprof(tmp <- tempfile())
writeAntaresH5(res3, "testWrite22.h5", "bli")#61 secondes sans groupe / MCyears
summaryRprof(tmp)



h5ls("testWrite22.h5")



