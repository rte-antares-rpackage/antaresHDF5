
# opts <- setSimulationPathH5()
# h5ReadAntares(path, select = "Out_addDownwardMargin", mcYears = 1)
# h5ReadAntares(path, select = "Out_addUpwardMargin", mcYears = 1)
# h5ReadAntares(path, select = "Out_addExportAndImport", mcYears = 1)
# h5ReadAntares(path, select = "Out_addLoadFactorLink", mcYears = 1)
# h5ReadAntares(path, select = "Out_externalDependency", mcYears = 1)
# h5ReadAntares(path, select = "Out_loadFactor", mcYears = 1)
# h5ReadAntares(path, select = "Out_modulation", mcYears = 1)
# h5ReadAntares(path, select = "Out_netLoadRamp", mcYears = 1)
# h5ReadAntares(path, select = "Out_surplus", mcYears = 1)
# h5ReadAntares(path, select = "Out_surplusClusters", mcYears = 1)
# addStraitments(opts,addDownwardMargin = TRUE)

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
                           ){

  # addDownwardMargin = TRUE
  # addUpwardMargin = TRUE
  # addExportAndImport = TRUE
  # addLoadFactorLink = TRUE
  # externalDependency = TRUE
  # loadFactor = TRUE
  # modulation = TRUE
  # netLoadRamp = TRUE
  # surplus = TRUE
  # surplusClusters = TRUE
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
    surplusClusters = surplusClusters)

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
    GP <- paste0(timeStep, "/", "areas", "/", mcY)
    .writeNewColumns(path = path,
                newdata = outToWrite$areas,
                GP = GP,
                namesVariable = columnsToAdd$areas)
  }


  if(writeLinks){
    GP <- paste0(timeStep, "/", "links", "/", mcY)
    .writeNewColumns(path = path,
                     newdata = outToWrite$links,
                     GP = GP,
                     namesVariable = columnsToAdd$links)
  }

  if(writeClusters){
    GP <- paste0(timeStep, "/", "clusters", "/", mcY)
    .writeNewColumns(path = path,
                     newdata = outToWrite$clusters,
                     GP = GP,
                     namesVariable = columnsToAdd$clusters)
  }

  if(writeDistricts){
    GP <- paste0(timeStep, "/", "districts", "/", mcY)
    .writeNewColumns(path = path,
                     newdata = outToWrite$districts,
                     GP = GP,
                     namesVariable = columnsToAdd$districts)
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
  h5set_extent(fid, datatype, c(newDim))
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
  as.character(pkgEnvAntareasH5$processDispo[pkgEnvAntareasH5$processDispo$fctname%in%
                                  names(which(unlist(allStraitments))),]$trtName)
}


.calcNewColumns <- function(res, allStraitments){
  if(allStraitments$addDownwardMargin){
    try({
    res <- addDownwardMargin(res)
    })
  }
  if(allStraitments$addUpwardMargin){
    try({
    res <- addUpwardMargin(res)
    })
  }
  if(allStraitments$addExportAndImport){
    try({
    res <- addExportAndImport(res)
    })
  }
  if(allStraitments$addLoadFactorLink){
    try({
    res <- addLoadFactorLink(res)
    })
  }
  if(allStraitments$externalDependency){
    try({
    res <- addNetLoad(res)
    })
    try({
    extDep <- externalDependency(res, timeStep =  "hourly")

    idC <- getIdCols(extDep)
    res$areas <- merge(res$areas, extDep, by = idC)
    })
  }
  if(allStraitments$loadFactor){
    try({
    loadFactor <- loadFactor(res, timeStep =  "hourly")
    idC <- getIdCols(loadFactor)
    res$clusters <- merge(res$clusters, loadFactor, by = idC)
    })
  }
  if(allStraitments$modulation){
    try({
    mod <- modulation(res, timeStep =  "hourly")

    idC <- getIdCols(mod)
    res$clusters <- merge(res$clusters, mod, by = idC)
    })
  }
  if(allStraitments$netLoadRamp){
    try({
    netLoadRamp <- netLoadRamp(res, timeStep =  "hourly")

    idC <- getIdCols(netLoadRamp)
    res$areas <- merge(res$areas, netLoadRamp, by = idC)
    })
  }
  if(allStraitments$surplus){
    try({
    surplus <- surplus(res, timeStep =  "hourly")

    idC <- getIdCols(surplus)
    res$areas <- merge(res$areas, surplus, by = idC)
    })
  }
  if(allStraitments$surplusClusters){
    try({
    surplusClusters <- surplusClusters(res, timeStep =  "hourly")

    idC <- getIdCols(surplusClusters)
    res$clusters <- merge(res$clusters, surplusClusters, by = idC)
    })
  }
  res
}
