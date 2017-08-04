# showAliases()
# path <- "testStudy.h5"
# res <- h5ReadAntares(path, select = "downwardMargin")
# dim(res$areas)
# dim(res$links)
#
# removeVirtualAreas(res, getAreas(c("psp out-2")))
#
# antaresProcessing::addDownwardMargin(res)
# dim(res$areas)
# dim(res$links)
#
#
#
#
# showAliases("downwardMargin")
# mydata <- readAntares(select = c("downwardMargin", "FLOW LIN."))
# mydata <- removeVirtualAreas(mydata, getAreas(c("psp out-2")))
# addDownwardMargin(mydata)
# names(mydata$areas)
#
# opts <- setSimulationPathH5(path)
#
#
# addStraitments(opts,

addStraitments <- function(opts,
                           addDownwardMargin = FALSE,
                           addUpwardMargin = FALSE,
                           addExportAndImport = FALSE,
                           addLoadFactorLink = FALSE,
                           externalDependency = FALSE,
                           loadFactor = FALSE,
                           modulation = FALSE,
                           netLoadRamp = FALSE,
                           surplus = FALSE,
                           surplusClusters = FALSE
                           #surplusSectors = FALSE
                           ){

  addDownwardMargin = TRUE
  addUpwardMargin = TRUE
  addExportAndImport = TRUE
  addLoadFactorLink = TRUE
  externalDependency = TRUE
  loadFactor = TRUE
  modulation = TRUE
  netLoadRamp = TRUE
  surplus = TRUE
  surplusClusters = TRUE
  #surplusSectors = TRUE

  allStraitments <- list(
    addDownwardMargin = addDownwardMargin,
    addUpwardMargin = addUpwardMargin,
    addExportAndImport = addExportAndImport,
    addLoadFactorLink = addLoadFactorLink,
    externalDependency = externalDependency,
    loadFactor = loadFactor,
    modulation = modulation,
    netLoadRamp = netLoadRamp,
    surplus = surplus,
    surplusClusters = surplusClusters)#,surplusSectors = surplusSectors

  columnsToAdd <- .getNewColumnsName(allStraitments)
  writeAreas <- ifelse(is.null(columnsToAdd$areas), FALSE, TRUE)
  writeLinks <- ifelse(is.null(columnsToAdd$links), FALSE, TRUE)
  writeClusters <- ifelse(is.null(columnsToAdd$clusters), FALSE, TRUE)
  writeDistricts <- ifelse(is.null(columnsToAdd$districts), FALSE, TRUE)

  select <- .getSelectalias(allStraitments)
  ##Load first Mcyear

  mcYear <- opts$mcYears
  outToWrite <- sapply(mcYear, function(X){
    res <- h5ReadAntares(path, select = select, mcYears = X)
    res <- .calcNewColumns(res, allStraitments)
    if(writeAreas){
      res$areas <- res$areas[, .SD, .SDcols = columnsToAdd$areas]
    }
    if(writeLinks){
      res$links <- res$links[, .SD, .SDcols = columnsToAdd$links]
    }
    if(writeClusters){
      res$clusters <- res$clusters[, .SD, .SDcols = columnsToAdd$clusters]
    }
    if(writeDistricts){
      res$districts <- res$districts[, .SD, .SDcols = columnsToAdd$districts]
    }
    res
  }, simplify = FALSE)

   outList <- names(outToWrite[[1]])

   outToWrite <- sapply(outList, function(X){
    as.matrix(rbindlist(lapply(outToWrite, function(Y){Y[[X]]})))
  })



  #path <- opts$h5path
   path <- "testStudy.h5"
  fid <- H5Fopen(path)

  ##Add control on straitments to define all this objects
  timeStep <- "hourly"
  mcY <- "mcInd"

  ##IfverWiteAreas

  if(writeAreas){
    GP <- paste0(timeStep, "/", dfStrait, "/", mcY)
    namesVariable <- variableToAddAreas
    .writeNewColumns(path = path,
                newdata = outToWrite$areas,
                GP = GP,
                namesVariable = columnsToAdd$areas)

    }

}



.getDim <- function(fid, GP, type = "size")
{
  did <- H5Dopen(fid, GP)
  H5Dget_space(did)
  res <- H5Dget_space(did)
  H5Dclose(did)
  dim <- H5Sget_simple_extent_dims(res)[[type]]
  dim
}

.getIndexToWrite <- function(dim, nbVarToWrite){
  list(1:dim[1], (dim[2] + 1) : (dim[2] + nbVarToWrite), 1:dim[3], 1:dim[4])
}


.writeNewColumns <- function(path, newdata, GP, namesVariable)
{
  fid <- H5Fopen(path)
  nbVarToWrite <- ncol(newdata)
  nbVarToWrite <- length(namesVariable)
  datatype <- paste0(GP, "/data")
  oldStruct <-  paste0(GP, "/structure/reCalcVar")
  structVarAdd <- h5read(path, oldStruct)
  structVarAdd[which(structVarAdd == "")[1:nbVarToWrite]] <- namesVariable
  h5write(structVarAdd, path, oldStruct)

  actualDim <- .getDim(fid, datatype)
  indexToWrite <- .getIndexToWrite(actualDim, nbVarToWrite)
  dimtowrite <- unlist(lapply(indexToWrite, length))
  arrayToWrite <- array(newdata, dimtowrite)
  newDim <- actualDim
  newDim[2] <- newDim[2] + dimtowrite[2]
  h5set_extent(fid, datatype, c(len1, newDim))
  h5writeDataset.array(obj = arrayToWrite, fid, datatype, index = indexToWrite)
}


.getNewColumnsName <- function(allStraitments)
{
  areas <- NULL
  links <- NULL
  clusters <- NULL
  districts <- NULL
  for(X in pkgEnvAntareasH5$processDispo$fctname){
    if(get(paste0("allStraitments"))[[X]]){
      areas <- c(areas, pkgEnvAntareasH5$process[[X]]$areas)
      links <- c(links, pkgEnvAntareasH5$process[[X]]$links)
      clusters <- c(clusters, pkgEnvAntareasH5$process[[X]]$clusters)
      districts <- c(districts, pkgEnvAntareasH5$process[[X]]$districts)

    }
  }
  list(areas = areas,
       links = links,
       clusters = clusters,
       districts = districts)
}

.getSelectalias <- function(allStraitments){
  as.character(pkgEnvAntareasH5$processDispo[pkgEnvAntareasH5$processDispo$trtName%in%
                                  names(which(unlist(allStraitments))),]$trtName)
}


.calcNewColumns <- function(res, allStraitments){
  if(allStraitments$addDownwardMargin){
    res <- addDownwardMargin(res)
  }
  if(allStraitments$addUpwardMargin){
    res <- addUpwardMargin(res)
  }
  if(allStraitments$addExportAndImport){
    res <- addExportAndImport(res)
  }
  if(allStraitments$addLoadFactorLink){
    res <- addLoadFactorLink(res)
  }
  if(allStraitments$externalDependency){
    res <- addNetLoad(res)
    extDep <- externalDependency(res, timeStep =  "hourly")
    idC <- getIdCols(extDep)
    res$areas <- merge(res$areas, extDep, by = idC)
  }
  if(allStraitments$loadFactor){
    loadFactor <- loadFactor(res, timeStep =  "hourly")
    idC <- getIdCols(loadFactor)
    res$clusters <- merge(res$clusters, loadFactor, by = idC)
  }
  if(allStraitments$modulation){
    mod <- modulation(res, timeStep =  "hourly")
    idC <- getIdCols(mod)
    res$clusters <- merge(res$clusters, mod, by = idC)
  }
  if(allStraitments$netLoadRamp){
    netLoadRamp <- netLoadRamp(res, timeStep =  "hourly")
    idC <- getIdCols(netLoadRamp)
    res$areas <- merge(res$areas, netLoadRamp, by = idC)
  }
  if(allStraitments$surplus){
    surplus <- surplus(res, timeStep =  "hourly")
    idC <- getIdCols(surplus)
    res$areas <- merge(res$areas, surplus, by = idC)
  }
  if(allStraitments$surplusClusters){
    surplusClusters <- surplusClusters(res, timeStep =  "hourly")
    idC <- getIdCols(surplusClusters)
    res$clusters <- merge(res$clusters, surplusClusters, by = idC)
  }
  res
}
