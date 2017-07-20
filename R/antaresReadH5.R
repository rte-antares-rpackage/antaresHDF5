#' Read data
#'
#' @export
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

  optimH5Read <- function(fid, index, DF){
    did <- H5Dopen(fid,  DF)
    if(is.null(index)){
      return(.Call("_H5Dread", did@ID, NULL, NULL,
                   NULL, FALSE, 0L , PACKAGE = "rhdf5"))
    }else{


      h5spaceFile <- H5Dget_space(did)
      maxSize <- .Call("_H5Sget_simple_extent_dims", h5spaceFile@ID, PACKAGE = "rhdf5")$maxsize
      len <- length(maxSize)
      K <-  sapply(len:1, function(X){
        if(is.null(index[[len-X + 1]])){
          seq_len(maxSize[X])
        }else{index[[len-X + 1]]}}
      )
      size <- unlist(lapply(K,length))
      h5spaceMem = H5Screate_simple(size)
      sid <- .Call("_H5Screate_simple", as.double(size), as.double(size), PACKAGE = "rhdf5")
      W <- H5Screate_simple(H5Sselect_index(h5spaceFile, K))@ID
      .Call("_H5Dread", did@ID, h5spaceFile@ID, W,
            NULL, FALSE, 0L , PACKAGE = "rhdf5")
    }

  }

  synthesis <- ifelse(is.null(mcYears), TRUE, FALSE)

  GP <- timeStep

  ##Open connection to h5 file
  fid <- H5Fopen(path)

  #Load attibutes
  did <- H5Dopen(fid, paste0(timeStep, "/attrib"))
  attrib <- unserialize(charToRaw(H5Dread(did)))
  H5Dclose(did)

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

    gid <- H5Gopen(fid,  paste0(GP, "/areas/", mcType, "/structure"))
    areasStruct <- h5dump(gid)
    H5Gclose(gid)
    #
    #     areasStruct <- h5read(path, paste0(GP, "/areas/", mcType, "/structure"))

    # H5close()
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
      areas <-  optimH5Read(fid = fid,
                            DF = paste0(GP, "/areas/", mcType, "/data"))
    }else{

      areas <- optimH5Read(fid = fid,
                           index = list(NULL, indexVar, indexArea, indexMC),
                           DF = paste0(GP, "/areas/", mcType, "/data"))

    }
    dimAreas <- dim(areas)
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

    tim <- getAllDateInfoFromDate(fid, timeStep)

    #Add time
    #areasData <- data.table(tim, areasData)
    areas[,c(names(tim)):=tim]
  }

  antaresRead:::.addClassAndAttributes(areas,
                                       synthesis,
                                       attrib$timeStep,
                                       attrib$opts,
                                       simplify = simplify, type = "areas")
  areas
}

