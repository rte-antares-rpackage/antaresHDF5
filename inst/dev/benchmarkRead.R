library(rhdf5)
library(data.table)

library(plyr)

##Read dataSet function
.readH5File <- function(VV,
                        allGroup,
                        expandRequest,
                        flags,
                        path,
                        index,
                        h5loc
){

  GP <- allGroup[VV]
  infoReq <- expandRequest[VV,]

  dfLoc <- .Call("_H5Dopen", h5loc, GP, NULL, PACKAGE = "rhdf5")
  h5group = new("H5IdComponent", ID = dfLoc)
  out <- .Call("_H5Dread", h5group@ID, NULL, NULL, NULL,
               TRUE, TRUE, PACKAGE = "rhdf5")

  out <- as.data.table.matrix.fast(out)
  out[,Mer := VV]
  out
}


path <- "bigStud.h5"
flags = h5default("H5F_ACC_RD")
flags = rhdf5:::h5checkConstants("H5F_ACC_RD", flags)

timeStep <- "hourly"
##Load attributes
attib <- h5dump(H5Gopen(H5Fopen(path), paste0(timeStep, "/attributes")),
                all = T, read.attributes = TRUE)


mcYears <- attib$opts$mcYears

##Put data to null
areasData <- NULL
linksData <- NULL
districtsData <- NULL
clustersData <- NULL
areas <- attib$opts$areaList
expandRequest <- expand.grid(areas, mcYears)
expandRequest$Var1 <- as.character(expandRequest$Var1)
allGroup <- sapply(1:nrow(expandRequest), function(X){
  paste(timeStep, "data/areas", paste(expandRequest[X,], collapse = "/"),sep = "/")
})

progressBar <- "text"
h5loc <- .Call("_H5Fopen", path, flags, PACKAGE = "rhdf5")


multGroup <- system.time(areasData <- rbindlist(plyr::llply(1:length(allGroup), .readH5File, allGroup = allGroup,
                                                            expandRequest = expandRequest,
                                                            flags = flags,
                                                            path = path,
                                                            index = index,
                                                            h5loc = h5loc
                                                            , .progress = progressBar, .parallel = FALSE)))


#
# h5createFile("testOneGroupe.h5")
# h5write(areasData, "testOneGroupe.h5", "To")
#
#
# h5createFile("testOneGroupe2.h5")
# h5write(areasData, "testOneGroupe2.h5", "To", level = 0)


flags = h5default("H5F_ACC_RD")
flags = rhdf5:::h5checkConstants("H5F_ACC_RD", flags)
h5loc <- .Call("_H5Fopen", "testOneGroupe.h5", flags, PACKAGE = "rhdf5")
dfLoc <- .Call("_H5Dopen", h5loc, "To", NULL, PACKAGE = "rhdf5")
h5group = new("H5IdComponent", ID = dfLoc)
monoGroupcompress <- system.time(outcp <- data.table(.Call("_H5Dread", h5group@ID, NULL, NULL, NULL,
                                                           TRUE, TRUE, PACKAGE = "rhdf5")))

flags = h5default("H5F_ACC_RD")
flags = rhdf5:::h5checkConstants("H5F_ACC_RD", flags)
h5loc <- .Call("_H5Fopen", "testOneGroupe2.h5", flags, PACKAGE = "rhdf5")
dfLoc <- .Call("_H5Dopen", h5loc, "To", NULL, PACKAGE = "rhdf5")
h5group = new("H5IdComponent", ID = dfLoc)
monoGroupNocompress <- system.time(out2 <- data.table(.Call("_H5Dread", h5group@ID, NULL, NULL, NULL,
                                                            TRUE, TRUE, PACKAGE = "rhdf5")))

system.time(test2 <- h5read("testOneGroupe2.h5", "To"))


system.time(datadump <- h5dump("testOneGroupe2.h5"))


#fwrite(out, "testBtxt.txt")
txtFile <- system.time(out <- fread("testBtxt.txt"))


library(antaresRead)
setSimulationPath("C:/Users/titorobe/Desktop/antaresStudy", 1)
Rprof(tmp <- tempfile())
readAntar <- system.time(are <- readAntares("all", mcYears = "all", select = c("LOLD", "OV. COST",
                                                                               "NODU")))
summaryRprof(tmp)


multGroup
monoGroupcompress
monoGroupNocompress
txtFile
readAntar





h5createFile("testOneGroupe4.h5")
for(i in 1:ncol(areasData))
{
  h5write(unlist(areasData[, .SD, .SDcols = i]), "testOneGroupe4.h5", paste0("To", i), level = 0)
  print(i)
}

system.time(for(i in 1:ncol(areasData))
{
  vec <- h5read("testOneGroupe4.h5", paste0("To", i))
})



h5createFile("testOneGroupe5.h5")
h5write(areasData, "testOneGroupe5.h5", "To", level = 0)

library(h5)
file <- h5file("test3.h5")
file["test/t3", compression = 0L] <- as.matrix(areasData)
dataset_testmat <- file["test/t3"]
system.time(R <- dataset_testmat[])
createDataSet(file, "test/t4", dim(areasData))

#Test fst
library(fst)
write.fst(areasData, "fst.fst")
system.time(re <- read.fst( "fst.fst", as.data.table = TRUE))
system.time(re2 <- fread("testBtxt.txt"))


H5close()
file.remove("tt5.h5")
h5createFile("tt5.h5")
h5createDataset("tt5.h5","tt2", level = 0, dims = dim(areasData))#, chunk = c(dim(areasData)[1]/length(unique(areasData$Mer)),1))
h5write(as.matrix(areasData), "tt5.h5", "tt2")
system.time(Vic <- h5read("tt5.h5", "tt2"))
system.time(vic2 <- h5read("tt5.h5", "tt2", index = list(1:dim(areasData)[1], 1:dim(areasData)[2])))




file.remove("tt6.h5")
h5createFile("tt6.h5")
for(i in unique(areasData$Mer)){
  antaresTP <- areasData[Mer == i]
  h5createDataset("tt6.h5",paste0("tt",i), level = 0, dims = dim(antaresTP), chunk = list(dim(antaresTP)[1],1))
  h5write(as.matrix(antaresTP),"tt6.h5", paste0("tt",i))
  print(i)
}





Vic <- list()
Rprof(tmp <- tempfile())
system.time(for(i in unique(areasData$Mer)){
  Vic[[i]] <- h5read("tt6.h5", paste0("tt",i))
})
summaryRprof(tmp)





flags = h5default("H5F_ACC_RD")
flags = rhdf5:::h5checkConstants("H5F_ACC_RD", flags)
h5loc <- .Call("_H5Fopen", "tt6.h5", flags, PACKAGE = "rhdf5")


Vic <- list()
Rprof(tmp <- tempfile())
system.time(for(i in unique(areasData$Mer)){
  dfLoc <- .Call("_H5Dopen", h5loc,  paste0("tt",i), NULL, PACKAGE = "rhdf5")
  h5group = new("H5IdComponent", ID = dfLoc)
  Vic[[i]] <- .Call("_H5Dread", h5group@ID, NULL, NULL, NULL,
                    TRUE, TRUE, PACKAGE = "rhdf5")
})
summaryRprof(tmp)


tpData <- unlist(areasData)
dim3 <- length(unique(areasData$Mer))
dim2 <- ncol(areasData)
dim1 <- nrow(areasData)/dim3
tpData <- unlist(areasData)



areasData


arR <- array(1:1000, dim = c(10,10,10))
h5createFile("array.h5")
h5write(arR,  "array.h5", "Array")
h5ls("array.h5")

areasData2 <- areasData
library(abind)
arR <- abind(split(areasData2, areasData2$Mer), along=3)
dim(arR)


H5close()
file.remove('arrayAll.h5')
h5createFile("arrayAll.h5")
h5createDataset("arrayAll.h5","ar", level = 0, dims = dim(arR), chunk = c(dim(arR)[1],1,1))
h5write(arR, "arrayAll.h5", "ar")
h5createDataset("arrayAll.h5","ar2", level = 0, dims = dim(arR))#, chunk = list(dim(arR)[1],1,1))
h5write(arR, "arrayAll.h5", "ar2")

h5createDataset("arrayAll.h5","ar3", level = 0, dims = dim(arR), chunk = c(dim(arR)[1],1,20))
h5write(arR, "arrayAll.h5", "ar3")

h5createDataset("arrayAll.h5","ar4", level = 0, dims = dim(arR), chunk = list(1,1,1))
h5write(arR, "arrayAll.h5", "ar4")

h5createDataset("arrayAll.h5","ar5", level = 4, dims = dim(arR), chunk = list(dim(arR)[1],1,1))
h5write(arR, "arrayAll.h5", "ar5")
h5createDataset("arrayAll.h5","ar6", level = 4, dims = dim(arR))#, chunk = list(dim(arR)[1],1,1))
h5write(arR, "arrayAll.h5", "ar6")

h5createDataset("arrayAll.h5","ar12", level = 4, dims = dim(arR), chunk = list(dim(arR)[1],1,1))
h5write(arR, "arrayAll.h5", "ar12")


#H5Pset_chunk_cache(h5group, 100, 10000, 0.5)
library(microbenchmark)

BM <- microbenchmark(R1 <- h5read("arrayAll.h5", "ar"),R2 <- h5read("arrayAll.h5", "ar2"))
library(ggplot2)
autoplot(BM)


BM <- microbenchmark(R1 <- h5read("arrayAll.h5", "ar", index = list(NULL, NULL, 1:100)),
                     R2 <- h5read("arrayAll.h5", "ar2", index = list(NULL, NULL, 1:100)))

autoplot(BM)


BM <- microbenchmark(R1 <- h5read("arrayAll.h5", "ar", index = list(NULL, 1:5, NULL)),
                     R2 <- h5read("arrayAll.h5", "ar2", index = list(NULL, 1:5, NULL)))

autoplot(BM)


BM <- microbenchmark(R1 <- h5read("arrayAll.h5", "ar", index = list(NULL, 1:5, 1:50)),
                     R2 <- h5read("arrayAll.h5", "ar2", index = list(NULL, 1:5, 1:50)))

h5read("arrayAll.h5", "ar", index = list(1, 1, 1))
h5read("arrayAll.h5", "ar6", index = list(1, 1, 1))


h5createDataset("arrayAll.h5","ar9", level = 0, dims = dim(res))#, chunk = list(dim(arR)[1],1,1))
h5write(as.matrix(res), "arrayAll.h5", "ar9")

h5createDataset("arrayAll.h5","ar10", level = 1, dims = dim(res))#, chunk = list(dim(arR)[1],1,1))
h5write(as.matrix(res), "arrayAll.h5", "ar10")


h5createDataset("arrayAll.h5","ar11", level = 1, dims = dim(res), chunk = list(dim(res)[1],1))
h5write(as.matrix(res), "arrayAll.h5", "ar11")

system.time(R8 <- h5read("arrayAll.h5", "ar10", index = list(NULL, 1)))
system.time(R8 <- h5read("arrayAll.h5", "ar11", index = list(NULL, 1)))


h5ls("arrayAll.h5")

system.time(h5read("arrayAll.h5", "ar9"))
system.time(R8 <- h5read("arrayAll.h5", "ar9", index = list(1:4000000, 1:25)))
system.time(re <- h5read("arrayAll.h5", "ar7", index = list(1,1)))
re


system.time(V <- h5read("arrayAll.h5", "ar12", index = list(NULL, 1, NULL)))



vv <-



BM <- microbenchmark({R1 <- h5read("arrayAll.h5", "ar", index = list(NULL, 1:5, 1:50))
                      H5close()},
                     {R4 <- h5read("arrayAll.h5", "ar5", index = list(NULL, 1:5, 1:50))
                     H5close()},
                     {R5 <- h5read("arrayAll.h5", "ar6", index = list(NULL, 1:5, 1:50))
                     H5close()})

autoplot(BM)

?H5Pset_chunk_cache


system.time(V <- h5read("arrayAll.h5", "ar", index = list(NULL, NULL, NULL)))






res <- rbindlist(apply(V, 3, function(X){as.data.table.matrix.fast(X)}))
?H5Pset_chunk_cache

arrayToData.table <- function(array, dim)
{
  W <- alply(array, dim, function(X)unlist(X, use.names = FALSE))
  setattr(W, "names", paste("V", 1:dim(array)[dim], sep = ""))
  dimS <- 1:length(dim(array))
  dimNot <- dimS[-dim]
  setattr(W, "row.names", .set_row_names(prod(dim(array)[dimNot])))
  setattr(W, "class", c("data.table", "data.frame"))
  alloc.col(W)
}

system.time(V <- h5read("arrayAll.h5", "ar"))
#system.time(V <- h5read("arrayAll.h5", "ar", index = list(NULL, 1:5, NULL)))


system.time(W <-arrayToData.table(V, 2))



Rprof(tmp <- tempfile())
system.time(res <- rbindlist(sapply(1:dim(V)[3], function(X)as.data.table.matrix.fast(V[,,X]), simplify = FALSE)))







summaryRprof(tmp)

h5write()

W <- as.vector(V)
X <- split(W, 1:31)
data.table(X)

W <- aperm(V)

W$V2 <- 1:2
X <- data.table::dcast(W, formula = . ~ V2)
dcast(ir, Sepal.Length~Species, value.var = "Sepal.Length")

cbind(split(W, 1:2))
melt(W )

test <- array(1:1000, c(10,10,10))

H5close()
file.remove("arraywritesequencaly.h5")
h5createFile("arraywritesequencaly.h5")
h5createDataset("arraywritesequencaly.h5","ar", level = 0, dims = dim(test), chunk = list(10,1,1))
for(i in 1:10){
  h5write(test[,,i], "arraywritesequencaly.h5", "ar", index = list(NULL, NULL, i))
}
h5read("arraywritesequencaly.h5", "ar")




test <- data.table(test)
test$tp <- 1:10
dcast(test, V1~tp, drop=FALSE, fill=0)



H5close()
file.remove('array4D.h5')
h5createFile("array4D.h5")
h5write(array(1:10000, c(10,10,10,10)), "array4D.h5", "ar")
h5ls("array4D.h5")


###Test avec cache
areasData2 <- areasData
library(abind)
arR <- abind(split(areasData2, areasData2$Mer), along=3)
dim(arR)


H5close()
file.remove('arraycache.h5')
h5createFile("arraycache.h5")
h5createDataset("arraycache.h5","ar", level = 0, dims = dim(arR), chunk = list(dim(arR)[1],1,1))
h5write(arR, "arraycache.h5", "ar")

pid <- H5Pcreate("H5P_DATASET_ACCESS")
H5Pset_chunk_cache(pid, 100, 10000, 0.5)

fid <- H5Fopen("arrayAll.h5")
did <- H5Dopen(fid, "ar")
H5Sselect_index(did, list(1:5, 1:5, 1:5))
res <- H5Dread(did)

space <- H5Dget_space(did)
res <- .Call("_H5Dread", did@ID, NULL, NULL, NULL,
             FALSE, FALSE, PACKAGE = "rhdf5")
