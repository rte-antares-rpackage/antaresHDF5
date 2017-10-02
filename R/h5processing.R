  # library(antaresHdf5)
  # library(antaresProcessing)
  # library(data.table)
  # devtools::load_all(".")
  # path <- "D:/Users/titorobe/Desktop/Antares/antaresHdf5"
  # opts <- setSimulationPathH5(path)
  # addStraitments(opts,addDownwardMargin = TRUE)
  # timeStep = "hourly"
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
  # opts <- setSimulationPath("D:/Users/titorobe/Desktop/Antares/antaresHdf5", 1)
  # mcY = "mcInd"
addStraitments <- function(opts,
                           mcY = "mcInd",
                           addDownwardMargin = FALSE,
                           addUpwardMargin = FALSE,
                           addExportAndImport = FALSE,
                           addLoadFactorLink = FALSE,
                           externalDependency = FALSE,
                           loadFactor = FALSE,
                           modulation = FALSE,
                           netLoadRamp = FALSE,
                           surplus = FALSE,
                           surplusClusters = FALSE,
                           evalAreas = list(),
                           evalLinks = list(),
                           evalClusters = list(),
                           evalDistricts = list(),
                           columnsToSelects = NULL){
  
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
  # surplusSectors = TRUE
  
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
  
  
  #select <- c(select, columnsToSelects)
  ##Load first Mcyear
  
  if(mcY == "mcInd")
  {
    mcYear <- opts$mcYears
  }
  if(mcY == "mcAll")
  {
    mcYear <- "mcAll"
  }
  timeStep <- "hourly"
  
  outToWrite <- sapply(mcYear, function(X){
    if(X == "mcAll"){
      X <- NULL
    }
    myOut <- .readDataEndAddColumn(opts, select = select, mcYears = X, timeStep = timeStep,
                                   evalAreas = evalAreas, evalLinks = evalLinks,
                                   evalClusters = evalClusters, evalDistricts = evalDistricts,
                                   columnsToSelects = columnsToSelects)
    outList <- names(myOut)
    outToWrite <- sapply(outList, function(HH){
      as.matrix(myOut[[HH]])
    })
    
    
    .writeAllTables(timeStep = timeStep,
                    mcY = mcY,
                    path = opts$h5path,
                    outToWrite = outToWrite ,
                    areas = writeAreas,
                    links = writeLinks,
                    clusters = writeClusters,
                    districts = writeDistricts,
                    mcYear = X, writeStruct = X == mcYear[1])
  }, simplify = FALSE)
  
  
  ##Add control on straitments to define all this objects
  
  ##IfverWiteAreas
  
  
  
  
  
  
}
.writeAllTables <- function(timeStep, mcY, path, outToWrite,
                            areas, links, clusters, districts, mcYear = NULL, writeStruct = FALSE){
  fid <- H5Fopen(path)
  sapply(c("areas", "links", "clusters", "districts"), function(X){
    print(X)
    if(get(X)){
      fid <- H5Fopen(path)
      Y <- eval(X)
      print(Y)
      GP <- paste0(timeStep, "/", Y, "/", mcY)
      .writeNewColumns(fid = fid,
                       newdata = outToWrite[[Y]],
                       GP = GP, mcYear = mcYear,
                       writeStruct = writeStruct)
    }
    
  })
  
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

.getIndexToWrite <- function(dim, nbVarToWrite, mcYear = NULL){
  d4 <- ifelse(is.null(mcYear), 1:dim[4], mcYear)
  list(1:dim[1], (dim[2] + 1) : (dim[2] + nbVarToWrite), 1:dim[3], d4)
}


.readDataEndAddColumn <- function(opts, select, mcYears, timeStep, 
                                  evalAreas, evalLinks,
                                  evalClusters, evalDistricts, columnsToSelects){
  res <- readAntares(opts = opts, select = c(select,columnsToSelects), mcYears = mcYears, timeStep = timeStep)
  for(i in 1:length(res)){
    res[[i]] <- res[[i]][, .SD, .SDcols = names(res[[i]])[!names(res[[i]])%in%select]]
  }
  res <- .calcNewColumns(res, allStraitments, timeStep = timeStep)
  if(writeAreas){
    if(length(evalAreas) > 0)
    {
      res$areas[, names(evalAreas) := lapply(evalAreas, function(X){eval(parse(text = X))})]
    }
    res$areas <- res$areas[, .SD, .SDcols = c(columnsToAdd$areas, names(evalAreas))]
  }
  if(writeLinks){
    if(length(evalLinks) > 0)
    {
      res$areas[, names(evalLinks) := lapply(evalLinks, function(X){eval(parse(text = X))})]
    }
    res$links <- res$links[, .SD, .SDcols = c(columnsToAdd$links, names(evalLinks))]
  }
  if(writeClusters){
    if(length(evalClusters) > 0)
    {
      res$areas[, names(evalClusters) := lapply(evalClusters, function(X){eval(parse(text = X))})]
    }
    res$clusters <- res$clusters[, .SD, .SDcols = c(columnsToAdd$clusters,names(evalClusters))]
  }
  if(writeDistricts){
    if(length(evalDistricts) > 0)
    {
      res$areas[, names(evalDistricts) := lapply(evalDistricts, function(X){eval(parse(text = X))})]
    }
    res$districts <- res$districts[, .SD, .SDcols = c(columnsToAdd$districts, names(evalDistricts))]
  }
  res
}


.writeNewColumns <- function(fid, newdata, GP, mcYear = NULL, writeStruct = FALSE)
{

  nbVarToWrite <- ncol(newdata)
  namesVariable <- colnames(newdata)
  datatype <- paste0(GP, "/data")
  if(writeStruct)
  {
    oldStruct <-  paste0(GP, "/structure/reCalcVar")
    
    did <- H5Dopen(fid, oldStruct)
    structVarAdd <- H5Dread(did )
    H5Dclose(did)
    structVarAdd[which(structVarAdd == "")[1:nbVarToWrite]] <- namesVariable
    #h5write(structVarAdd, path, oldStruct)
    h5writeDataset(obj = structVarAdd,  fid, oldStruct)
  }
  
  actualDim <- .getDim(fid, datatype)
  indexToWrite <- .getIndexToWrite(actualDim, nbVarToWrite, mcYear)
  dimtowrite <- unlist(lapply(indexToWrite, length))
  arrayToWrite <- array(newdata, dimtowrite)
  newDim <- actualDim
  newDim[2] <- newDim[2] + dimtowrite[2]
  h5set_extent(fid, datatype, c(newDim))
  h5writeDataset.array(obj = arrayToWrite, fid, datatype, index = indexToWrite)
  H5close()
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


.calcNewColumns <- function(res, allStraitments, timeStep){
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
      extDep <- externalDependency(res, timeStep =  timeStep)
      
      idC <- getIdCols(extDep)
      res$areas <- merge(res$areas, extDep, by = idC)
    })
  }
  if(allStraitments$loadFactor){
    try({
      loadFactor <- loadFactor(res, timeStep =  timeStep)
      idC <- getIdCols(loadFactor)
      res$clusters <- merge(res$clusters, loadFactor, by = idC)
    })
  }
  if(allStraitments$modulation){
    try({
      mod <- modulation(res, timeStep =  timeStep)
      
      idC <- getIdCols(mod)
      res$clusters <- merge(res$clusters, mod, by = idC)
    })
  }
  if(allStraitments$netLoadRamp){
    try({
      netLoadRamp <- netLoadRamp(res, timeStep = timeStep)
      
      idC <- getIdCols(netLoadRamp)
      res$areas <- merge(res$areas, netLoadRamp, by = idC)
    })
  }
  if(allStraitments$surplus){
    try({
      surplus <- surplus(res, timeStep = timeStep)
      
      idC <- getIdCols(surplus)
      res$areas <- merge(res$areas, surplus, by = idC)
    })
  }
  if(allStraitments$surplusClusters){
    try({
      surplusClusters <- surplusClusters(res, timeStep =  timeStep)
      idC <- getIdCols(surplusClusters)
      res$clusters <- merge(res$clusters, surplusClusters, by = idC)
    })
  }
  res
}

