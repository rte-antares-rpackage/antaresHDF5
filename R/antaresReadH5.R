#' Read data
#'
#' @param path {character} path of h5file to load
#' @param areas see \link[antaresRead]{readAntares}
#' @param links see \link[antaresRead]{readAntares}
#' @param clusters see \link[antaresRead]{readAntares}
#' @param districts see \link[antaresRead]{readAntares}
#' @param mcYears see \link[antaresRead]{readAntares}
#' @param timeStep see \link[antaresRead]{readAntares}
#' @param select see \link[antaresRead]{readAntares}
#' @param showProgress see \link[antaresRead]{readAntares}
#' @param simplify see \link[antaresRead]{readAntares}
#' @param perf \code{boolean}, eval performance during developpement time, to remove
#'
#' @export
h5ReadAntares <- function(path, areas = NULL, links = NULL, clusters = NULL,
                          districts = NULL, mcYears = NULL,
                          timeStep = "hourly", select = "all", showProgress = TRUE,
                          simplify = TRUE, perf = TRUE){

  if(perf){
    Beg <- Sys.time()
  }

  synthesis <- ifelse(is.null(mcYears), TRUE, FALSE)
  GP <- timeStep

  ##Open connection to h5 file
  fid <- H5Fopen(path)


  #Load attibutes

  if(H5Lexists(fid, paste0(timeStep, "/attrib")))
  {

    did <- H5Dopen(fid, paste0(timeStep, "/attrib"))
    attrib <- unserialize(charToRaw(H5Dread(did)))
    H5Dclose(did)

    if(!is.null(attrib$opts$linksDef)){
      attrib$opts$linksDef <- data.table(attrib$opts$linksDef)
    }
    if(!is.null(attrib$opts$districtsDef)){
      attrib$opts$districtsDef <- data.table(attrib$opts$districtsDef)
    }
  }else{
    attrib <- NULL
  }
  if(is.null(mcYears)){
    mcType <- "mcAll"
    mcYears <- "mcAll"
  }else{
    mcType <- "mcInd"
  }



  ##Load areas
  listOut <- list()
  areas <- .loadAreas(areas = areas,
                      fid = fid,
                      select = select,
                      mcYears = mcYears,
                      GP = GP,
                      mcType = mcType,
                      synthesis = synthesis,
                      simplify = simplify,
                      attrib = attrib)
  if(!is.null(areas)){
    listOut$areas <- areas
    rm(areas)
  }

  links <- .loadLinks(links = links,
                      fid = fid,
                      select = select,
                      mcYears = mcYears,
                      GP = GP,
                      mcType = mcType,
                      synthesis = synthesis,
                      simplify = simplify,
                      attrib = attrib)

  if(!is.null(links)){
    listOut$links <- links
    rm(links)
  }


  districts <- .loadDistricts(districts = districts,
                              fid = fid,
                              select = select,
                              mcYears = mcYears,
                              GP = GP,
                              mcType = mcType,
                              synthesis = synthesis,
                              simplify = simplify,
                              attrib = attrib)

  if(!is.null(districts)){
    listOut$districts <- districts
    rm(districts)
  }


  clusters <- .loadClusters(clusters = clusters,
                            fid = fid,
                            select = select,
                            mcYears = mcYears,
                            GP = GP,
                            mcType = mcType,
                            synthesis = synthesis,
                            simplify = simplify,
                            attrib = attrib)

  if(!is.null(clusters)){
    listOut$clusters <- clusters
    rm(clusters)
  }


  if(length(listOut) == 1){

    if(perf){
      TotalTime <-Sys.time() - Beg
      cat(paste0("Time for loading : ", round(TotalTime, 3), "\n"))
      objectS <- object.size(listOut)/1024^2
      cat(paste0("Size of object loaded : ", round(objectS, 1), "Mo\n"))
      cat(paste0("Mo/s loaded : ",round(as.numeric(objectS)/ as.numeric(TotalTime),1), "\n"))
      dtaloded <- sum(unlist(lapply(listOut, function(X)prod(dim(X)))))
      cat(paste0("Data loded/s : ", round(dtaloded/ as.numeric(TotalTime) / 1000000, 2), " Millions", "\n"))
    }

    listOut[[1]]
  }else{
    listOut <- .addClassAndAttributes(listOut, synthesis, timeStep,
                                      attrib$opts, simplify)
    if(perf){
      TotalTime <-Sys.time() - Beg
      cat(paste0("Time for loading : ", round(TotalTime, 3), "\n"))
      objectS <- object.size(listOut)/1024^2
      cat(paste0("Size of object loaded : ", round(objectS, 1), "Mo\n"))
      cat(paste0("Mo/s loaded : ",round(as.numeric(objectS)/ as.numeric(TotalTime),1), "\n"))
      dtaloded <- sum(unlist(lapply(listOut, function(X)prod(dim(X)))))
      cat(paste0("Data loded/s : ", round(dtaloded/ as.numeric(TotalTime) / 1000000, 2), " Millions", "\n"))
    }

    listOut
  }
}




#' Transform array to data.table
#'
#' @param array \code{array}, array od data to transform
#'
#' @return data.table
#'
#' @noRd
.arrayToDataTable <- function(array)
{
  dim <- 2
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

#' Load antares simulation data
#'
#' @param fid \code{H5IdComponent} id of h5 file open which \link{rhdf5::H5Fopen}
#' @param index \code{list} index of data to load
#' @param GP \code{character} name of group to load
#'
#' @noRd
.optimH5Read <- function(fid, index = NULL, GP){
  did <- H5Dopen(fid,  GP)
  if(is.null(index)){
    return(.Call("_H5Dread", did@ID, NULL, NULL,
                 NULL, FALSE, 0L, FALSE , PACKAGE = "rhdf5"))
  }else{

    h5spaceFile <- H5Dget_space(did)
    maxSize <- .Call("_H5Sget_simple_extent_dims", h5spaceFile@ID, PACKAGE = "rhdf5")$size
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
          NULL, FALSE, 0L , FALSE, PACKAGE = "rhdf5")
  }

}

#' Give request stucture
#'
#' @param type \code{character} type of request, must be area, link, cluster or district
#' @param selectedRow \code{character} selectoin on raw (country, link, cluster ....)
#' @param selectedCol \code{character} columns to select
#' @param fid \code{H5IdComponent} id of h5 file open which \link[rhdf5]{H5Fopen}
#' @param GP \code{character} name of data.frame to load
#' @param mcType \code{character}, must be mcInd or mcAll
#' @param mcYears \code{numeric or character} mcYears to laod
#'
#' @noRd
.makeStructure <- function(type = "area", selectedRow,
                           selectedCol, fid, GP, mcType, mcYears){
  typeS <- paste0(type, "s")

  gid <- H5Gopen(fid,  paste0(GP, "/", typeS, "/", mcType, "/structure"))

  struct <- h5dump(gid)

  H5Gclose(gid)
  compname <- NULL
  if(type == "cluster"){
    splitClust <- strsplit(struct[[type]], "/")
    clusterClean <- unlist(lapply(splitClust, function(X){X[1]}))
    struct[[type]] <- clusterClean
    compname <- unlist(lapply(splitClust, function(X){X[2]}))

  }


  if(selectedRow[1] == "all"){
    indexType  <- NULL
    Name <- struct[[type]]
  }else{
    indexType <- which(struct[[type]] %in% selectedRow)
    Name <- struct[[type]][indexType]
    if(type == "cluster"){
      compname <- compname[indexType]
    }
  }
  if(selectedCol == "all"){
    indexVar <- NULL
    varKeep <- struct$variable
  }else{
    indexVar <- which(struct$variable %in% selectedCol)
    indexVar <- unique(c(1, indexVar))
    varKeep <- struct$variable[indexVar]
  }
  if(mcYears[1] == "all"){
    indexMC <- NULL
    mcyLoad <- struct$mcYear
  }else{
    if(mcYears[1] == "mcAll"){
      indexMC <- NULL
      mcyLoad <- struct$mcYear
    }else{
      indexMC <- which(struct$mcYear %in% mcYears)
      mcyLoad <- struct$mcYear[indexMC]
    }
  }
  return(list(Name = Name, varKeep = varKeep, index = list(NULL, indexVar, indexType, indexMC),
              mcyLoad = mcyLoad, compname = compname))
}




#' Load areas
#'
#' @param areas \code{character}, area(s) to load
#' @param fid \code{H5IdComponent} id of h5 file open which \link[rhdf5]{H5Fopen}
#' @param select \code{character} columns to select
#' @param mcYears \code{numeric or character} mcYears to laod
#' @param GP \code{character} name of data.frame to load
#' @param mcType \code{character}, must be mcInd or mcAll
#' @param synthesis \code{boolean}
#' @param simplify \code{boolean}
#'
#' @noRd
.loadAreas <- function(areas,
                       fid,
                       select,
                       mcYears,
                       GP,
                       mcType,
                       synthesis,
                       simplify,
                       attrib){


  if(!is.null(areas)){

    if(H5Lexists(fid, paste0(GP, "/areas/", mcType, "/structure")))
    {

      struct  <- .makeStructure(type = "area",
                                selectedRow = areas,
                                selectedCol = select,
                                fid = fid,
                                GP = GP,
                                mcType = mcType,
                                mcYears = mcYears)


      if(all(unlist(lapply(struct$index, is.null)))){
        areas <-  .optimH5Read(fid = fid,
                               GP = paste0(GP, "/areas/", mcType, "/data"))
      }else{
        areas <- .optimH5Read(fid = fid,
                              index = struct$index,
                              GP = paste0(GP, "/areas/", mcType, "/data"))

      }


      #Format array
      areas <- .formatArray(data = areas, struct = struct, nameColumns = "area", mcType = mcType)

      #Add time
      tim <- getAllDateInfoFromDate(fid, GP)
      areas[,c(names(tim)):=tim]

      .addClassAndAttributes(areas,
                             synthesis,
                             attrib$timeStep,
                             attrib$opts,
                             simplify = simplify, type = "areas")
      areas
    }else{
      message("No data corresponding to your areas query.")
      return(NULL)
    }
  }else{NULL}}

#' Load links
#'
#' @param links \code{character}, link(s) to load
#' @param fid \code{H5IdComponent} id of h5 file open which \link[rhdf5]{H5Fopen}
#' @param select \code{character} columns to select
#' @param mcYears \code{numeric or character} mcYears to laod
#' @param GP \code{character} name of data.frame to load
#' @param mcType \code{character}, must be mcInd or mcAll
#' @param synthesis \code{boolean}
#' @param simplify \code{boolean}
#'
#' @noRd
.loadLinks <- function(links,
                       fid,
                       select,
                       mcYears,
                       GP,
                       mcType,
                       synthesis,
                       simplify,
                       attrib){
  ##Load links
  if(!is.null(links)){

    if(H5Lexists(fid, paste0(GP, "/links/", mcType, "/structure")))
    {


      struct  <- .makeStructure(type = "link",
                                selectedRow = links,
                                selectedCol = select,
                                fid = fid,
                                GP = GP,
                                mcType = mcType,
                                mcYears = mcYears)


      if(all(unlist(lapply(struct$index, is.null)))){
        links <-  .optimH5Read(fid = fid,
                               GP = paste0(GP, "/links/", mcType, "/data"))
      }else{
        links <- .optimH5Read(fid = fid,
                              index = struct$index,
                              GP = paste0(GP, "/links/", mcType, "/data"))

      }

      #Format array
      links <- .formatArray(data = links, struct = struct, nameColumns = "link", mcType = mcType)

      #Add time
      tim <- getAllDateInfoFromDate(fid, GP)
      links[,c(names(tim)):=tim]

      .addClassAndAttributes(links,
                             synthesis,
                             attrib$timeStep,
                             attrib$opts,
                             simplify = simplify, type = "links")
      links
    }else{
      message("No data corresponding to your links query.")
      return(NULL)
    }
  }else{NULL}
}



#' Load districts
#'
#' @param districts \code{character}, district(s) to load
#' @param fid \code{H5IdComponent} id of h5 file open which \link[rhdf5]{H5Fopen}
#' @param select \code{character} columns to select
#' @param mcYears \code{numeric or character} mcYears to laod
#' @param GP \code{character} name of data.frame to load
#' @param mcType \code{character}, must be mcInd or mcAll
#' @param synthesis \code{boolean}
#' @param simplify \code{boolean}
#'
#' @noRd
.loadDistricts <- function(districts,
                           fid,
                           select,
                           mcYears,
                           GP,
                           mcType,
                           synthesis,
                           simplify,
                           attrib){
  if(!is.null(districts)){

    if(H5Lexists(fid, paste0(GP, "/districts/", mcType, "/structure")))
    {

      struct  <- .makeStructure(type = "district",
                                selectedRow = districts,
                                selectedCol = select,
                                fid = fid,
                                GP = GP,
                                mcType = mcType,
                                mcYears = mcYears)


      if(all(unlist(lapply(struct$index, is.null)))){
        districts <-  .optimH5Read(fid = fid,
                                   GP = paste0(GP, "/districts/", mcType, "/data"))
      }else{
        districts <- .optimH5Read(fid = fid,
                                  index = struct$index,
                                  GP = paste0(GP, "/districts/", mcType, "/data"))

      }


      districts <- .formatArray(data = districts, struct = struct, nameColumns = "district", mcType = mcType)

      tim <- getAllDateInfoFromDate(fid, GP)

      #Add time
      districts[,c(names(tim)):=tim]

      .addClassAndAttributes(districts,
                             synthesis,
                             attrib$timeStep,
                             attrib$opts,
                             simplify = simplify, type = "districts")
      districts
    }else{
      message("No data corresponding to your districts query.")
      return(NULL)
    }}else{NULL}
}




#' Load clusters
#'
#' @param clusters \code{character}, cluster(s) to load
#' @param fid \code{H5IdComponent} id of h5 file open which \link{rhdf5::H5Fopen}
#' @param select \code{character} columns to select
#' @param mcYears \code{numeric or character} mcYears to laod
#' @param GP \code{character} name of data.frame to load
#' @param mcType \code{character}, must be mcInd or mcAll
#' @param synthesis \code{boolean}
#' @param simplify \code{boolean}
#'
#' @noRd
.loadClusters <- function(clusters,
                          fid,
                          select,
                          mcYears,
                          GP,
                          mcType,
                          synthesis,
                          simplify,
                          attrib){
  if(!is.null(clusters)){

    if(H5Lexists(fid, paste0(GP, "/clusters/", mcType, "/structure")))
    {


      struct  <- .makeStructure(type = "cluster",
                                selectedRow = clusters,
                                selectedCol = select,
                                fid = fid,
                                GP = GP,
                                mcType = mcType,
                                mcYears = mcYears)

      if(all(unlist(lapply(struct$index, is.null)))){
        clusters <-  .optimH5Read(fid = fid,
                                  GP = paste0(GP, "/clusters/", mcType, "/data"))
      }else{
        clusters <- .optimH5Read(fid = fid,
                                 index = struct$index,
                                 GP = paste0(GP, "/clusters/", mcType, "/data"))

      }


      dimclusters <- dim(clusters)
      clusters <- .formatArray(data = clusters, struct = struct, nameColumns = "area", mcType = mcType)

      compname <- as.factor(struct$compname)
      clusters[, cluster:= rep(rep(compname, each = dimclusters[1]), dimclusters[4])]
      tim <- getAllDateInfoFromDate(fid, GP)

      #Add time
      clusters[,c(names(tim)):=tim]

      .addClassAndAttributes(clusters,
                             synthesis,
                             attrib$timeStep,
                             attrib$opts,
                             simplify = simplify, type = "clusters")
      clusters
    }else{
      message("No data corresponding to your clusters query.")
      return(NULL)
    }}else{NULL}
}

#' Add structure information to data
#'
#' @param data {data.table} data load
#' @param struct {list}
#' @param nameColumns {character} column names
#' @param mcType {character}, must be mcInd, and mcAll
#'
#' @noRd
.formatArray <- function(data, struct, nameColumns, mcType){
  dimData <- dim(data)
  data <- .arrayToDataTable(data)
  nameS <- struct$varKeep
  names(data) <- nameS
  dataName <- as.factor(struct$Name)
  data[, c(nameColumns[1]):= rep(rep(dataName, each = dimData[1]), dimData[4])]
  if(mcType == "mcInd")
  {
    data[, mcYear := rep(struct$mcyLoad, each = dimData[1] * dimData[3])]
  }
  data
}



