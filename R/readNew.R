Rprof(tmp <- tempfile())
library(rhdf5)
library(data.table)
library(h5)

f <- h5file(path)


path <- "testNewFormat3.h5"



h5ls(path)

###Code enregistrement d'attributs (passage en binaire + h5write)
s <- serialize(attib, NULL, ascii = TRUE)
h5write(rawToChar(s), "testF1.h5", "tt")


system.time(s1 <- unserialize(charToRaw(h5read("testF1.h5", "tt"))))
identical(s1, attib)





attib
H5close()
file.remove("testF1.h5")
h5createFile("testF1.h5")
h5save("attib", file = "testF1.h5", name = "attrib")

tmp <- tempfile()

saveRDS(attib, file="test.rds", ascii = TRUE)

readRDS("test.rds")


loadx <- function(x, file) {
  load(file)
  return(x)
}

KW <- loadx(attib, "test.rda")


setSimulationPath("E:/DES/antaresFlowbased/TestrunSimulationFb/antaresStudy")
system.time(W <- readAntares(areas = "all" ,mcYears = 1, simplify = TRUE, select = "NODU"))
W


system.time(V <- h5ReadAntares(path,
                               areas = c("all"),
                               mcYears = 1, select = "NODU"))


identical(W, V)
#
# identical(W$`OV. COST`,V$`OV. COST`)
#
# for(i in 1:ncol(W))
# {
# print(names(W)[i])
#   print(
#   identical(W[, .SD, .SDcols = i],
#           V[, .SD, .SDcols = i]))
# }
#
#
# R1 <- unlist(W[, .SD, .SDcols = 1])
# R2 <- unlist(V[, .SD, .SDcols = 1])
# R1
# R2
#
# Ti <- attributes(V)
# Fr <- attributes(W)
# identical(Ti, Fr)
#
#
# attributes(Ti$opts$districtsDef)
# attributes(Fr$opts$districtsDef)
# identical(Ti$opts$districtsDef, Fr$opts$districtsDef)
#


h5ReadAntares <- function(path, areas = NULL, links = NULL, clusters = NULL,
                          districts = NULL, mcYears = NULL,
                          timeStep = "hourly", select = "all", showProgress = TRUE,
                          simplify = TRUE){

  arrayToDataTable <- function(array, dim = 2)
  {
    ecraseDim <- dim(array)[dim]
    dimS <- 1:length(dim(array))
    dimNot <- dimS[-dim]
    prodDim <- prod(dim(array)[dimNot])
    arrayL <- list()
    for(i in 1:dim(array)[2]){
      arrayL[[i]] <- unlist(array[,i,,])
    }
    setattr(arrayL, "names", paste("V", 1:ecraseDim, sep = ""))
    setattr(arrayL, "row.names", .set_row_names(prodDim))
    setattr(arrayL, "class", c("data.table", "data.frame"))
    alloc.col(arrayL)
  }

  synthesis <- ifelse(is.null(mcYears), TRUE, FALSE)

  GP <- timeStep

  ##Load attibutes
  attrib <- unserialize(charToRaw(h5read(path, paste0(timeStep, "/attrib"))))
  if(!is.null(attrib$opts$linksDef)){
    attrib$opts$linksDef <- data.table(attrib$opts$linksDef)
  }
  if(!is.null(attrib$opts$districtsDef)){
    attrib$opts$districtsDef <- data.table(attrib$opts$districtsDef)
  }


  if(is.null(mcYears)){
    mcType <- "mcAll"
    mcYears <- "mcAll"
  }else{
    mcType <- "mcInd"
  }

  if(!is.null(areas)){
    areasStruct <- h5read(path, paste0(GP, "/areas/", mcType, "/structure"))

    H5close()
    if(areas[1] == "all"){
      indexArea  <- NULL
      areaName <- areasStruct$area
    }else{
      indexArea <- which(areasStruct$area %in% areas)
      areaName <- areasStruct$area[indexArea]
    }

    if(select == "all"){
      indexVar <- NULL
    }else{
      indexVar <- which(areasStruct$variable %in% select)
      indexVar <- unique(c(1, indexVar))
    }
    if(mcYears[1] == "all"){
      indexMC <- NULL
      mcyLoad <- areasStruct$mcYear
    }else{
      if(mcYears[1] == "mcAll"){
        indexMC <- NULL
        mcyLoad <- areasStruct$mcYear
      }else{
      indexMC <- which(areasStruct$mcYear %in% mcYears)
      mcyLoad <- areasStruct$mcYear[indexMC]
      }
    }
    if(is.null(indexArea) & is.null(indexVar) & is.null(indexMC)){
      areas <- h5read(path, paste0(GP, "/areas/", mcType, "/data"))
      H5close()
    }else{
      areas <- h5read(path, paste0(GP, "/areas/", mcType, "/data"), index = list(NULL,
                                                                                 indexVar,
                                                                                 indexArea,
                                                                                 indexMC))
    }
    dimAreas <- dim(areas)
    H5close()
    areas <- arrayToDataTable(areas)

    if(select == "all"){
      nameS <- areasStruct$variable
    }else{
      nameS <- areasStruct$variable[indexVar]
    }
    names(areas) <- nameS
    areaName <- as.factor(areaName)
    areas[, area := rep(rep(areaName, each = dimAreas[1]), dimAreas[4])]

    if(mcType == "mcInd")
    {
      areas[, mcYear := rep(mcyLoad, each = dimAreas[1] * dimAreas[3])]
    }

    tim <- getAllDateInfoFromDate(path, timeStep)

    #Add time
    #areasData <- data.table(tim, areasData)
    areas[,c(names(tim)):=tim]
  }
  areas
  antaresRead:::.addClassAndAttributes(areas,
                                       synthesis,
                                       attrib$timeStep,
                                       attrib$opts,
                                       simplify = simplify, type = "areas")
  areas
}

