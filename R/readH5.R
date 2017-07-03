
#' Read the data of an Antares simulation
#'
#' @description
#' \code{\link{readAntaresH5}} Has the same behavior than \code{\link{antaresRead::readAntares}} but it based on h5 file.
#'
#' @param path \code{character}, path of h5 file to read.
#' @param areas
#'   Vector containing the names of the areas to import. If
#'   \code{NULL} no area is imported. The special value \code{"all"} tells the
#'   function to import all areas. By default, the value is "all" when no other argument is enter and "NULL" when other arguments are enter.
#' @param links
#'   Vector containing the name of links to import. If \code{NULL} no
#'   area is imported. The special value \code{"all"} tells the function to
#'   import all areas. Use function \code{\link{antaresRead::getLinks}} to import all links
#'   connected to some areas.
#' @param clusters
#'   Vector containing the name of the areas for which you want to
#'   import results at cluster level. If \code{NULL} no cluster is imported. The
#'   special value \code{"all"} tells the function to import clusters from all
#'   areas.
#' @param districts
#'   Vector containing the names of the districts to import. If \code{NULL},
#'   no district is imported. The special value \code{"all"} tells the function to import all
#'   districts.
#' @param select
#'   Character vector containing the name of the columns to import. If this
#'   argument is \code{NULL}, all variables are imported. Special names
#'   \code{"allAreas"} and \code{"allLinks"} indicate to the function to import
#'   all variables for areas or for links.
#' @param mcYears
#'   Index of the Monte-Carlo years to import. If \code{NULL}, synthetic results
#'   are read, else the specified Monte-Carlo simulations are imported. The
#'   special value \code{all} tells the function to import all Monte-Carlo
#'   simulations.
#' @param timeStep
#'   Resolution of the data to import: hourly (default), daily,
#'   weekly, monthly or annual.
#' @param showProgress
#'   If TRUE the function displays information about the progress of the
#'   importation.
#'
#'
#' @return An object of class "antaresDataList" is returned. It is a list of
#' data.tables, each element representing one type of element (areas, links,
#' clusters)
#'
#' @export
#'
readAntaresH5 <- function(path, areas = NULL, links = NULL, clusters = NULL,
                          districts = NULL, mcYears = NULL,
                          timeStep = "hourly", select = "all", showProgress = TRUE){


  ##Controle input before reading
  progressBar <- ifelse(showProgress, "text", "none")
  if(is.null(areas) & is.null(links) & is.null(clusters) & is.null(districts)){
    areas <- "all"
  }

  ##Read dataSet function
  .readH5File <- function(VV,
           allGroup,
           expandRequest,
           flags,
           path,
           index
  ){
    # library(rhdf5)
    # library(data.table)
    GP <- allGroup[VV]
    infoReq <- expandRequest[VV,]


    ##Use .call function directly, (faster)

    h5loc <- .Call("_H5Fopen", path, flags, PACKAGE = "rhdf5")
    dfLoc <- .Call("_H5Dopen", h5loc, GP, NULL, PACKAGE = "rhdf5")
    h5group = new("H5IdComponent", ID = dfLoc)
    #
    # H5Dget_space(dfLoc)
    #
    # out <- .Call("_H5Dread", dfLoc, NULL, NULL, NULL,
    #       FALSE, FALSE, PACKAGE = "rhdf5")
    #
    out <- rhdf5:::h5readDataset(h5group, list(NULL,index))
    out <- data.table(infoReq, out)
    H5close()
    out
  }


  if(select[1] == "all"){
    index <- NULL
  }

  #rhdf5, flags
  flags = h5default("H5F_ACC_RD")
  flags = rhdf5:::h5checkConstants("H5F_ACC_RD", flags)


  ##Load attributes
  attib <- h5dump(H5Gopen(H5Fopen(path), paste0(timeStep, "/attributes")),
                 all = T, read.attributes = TRUE)
  attib <- giveFormat(attib)
  class(attib$opts) <- "simOptions"
  ##

  ##Recupe struct name
  structName <- ifelse(is.null(mcYears), "structureMcall", "structure")


  #Select MCyears
  if(is.null(mcYears)){
    mcYears <- "mcAll"
  }
  synthesis <- ifelse(mcYears[1] == "mcAll", TRUE, FALSE)


  #Select MCyears
  if(mcYears[1] == "all"){
    mcYears <- attib$opts$mcYears
  }

  ##Put data to null
  areasData <- NULL
  linksData <- NULL
  districtsData <- NULL
  clustersData <- NULL


  ##Read areas
  if(!is.null(areas)){

    if(areas == "all"){
      areas <- attib$opts$areaList
    }

    expandRequest <- expand.grid(areas, mcYears)
    allGroup <- apply(expandRequest, 1, function(X){
      paste(timeStep, "data/areas",paste(X, collapse = "/"),sep = "/")
    })


    #Read structure
    H5close()
    struct <- h5readAttributes(path, paste0(timeStep, "/data/areas"))[[structName]]
    H5close()
    struct

    if(!select[1] == "all"){
      index <- c(1, which(struct[3:length(struct)]%in%select))
      struct <- struct[c(1:2, index+2)]
    }

    #Essai parallel, non concluant
    # nodes <- detectCores() - 1
    # cl <- makeCluster(nodes)
    # registerDoParallel(cl)
    if(showProgress){
      cat("Importing areas\n")
    }

    areasData <- rbindlist(plyr::llply(1:length(allGroup), .readH5File, allGroup = allGroup,
    expandRequest = expandRequest,
    flags = flags,
    path = path,
    index = index
    , .progress = progressBar, .parallel = FALSE))


    ##Give names
    setnames(areasData, names(areasData), struct)
    tim <- getAllDateInfoFromDate(path, timeStep)

    #Add time
    areasData <- data.table(tim, areasData)
    areasData[,timeId := as.integer(timeId)]
    # setattr(areasData, "class", c("antaresDataTable", "antaresData", "data.table", "data.frame"))
    # sapply(names(attib), function(X){
    #   setattr(areasData, X, attib[[X]])
    # })
    # areasData


    #Add attribS
    areasData <- antaresRead:::.addClassAndAttributes(areasData,
                                                      synthesis,
                                                 attib$timeStep,
                                                 attib$opts,
                                                 simplify = TRUE, type = "areas")

    if(mcYears[1] == "mcAll"){
      areasData[,mcYear := NULL]
    }

    areasData
  }

  #Links
  if(!is.null(links)){
    if(links == "all"){
      links <- attib$opts$linkList

    }

    #Read structure
    H5close()
    struct <- h5readAttributes(path, paste0(timeStep, "/data/links"))[[structName]]
    H5close()
    struct

    if(!select[1] == "all"){
      index <- c(1, which(struct[3:length(struct)]%in%select))
      struct <- struct[c(1:2, index+2)]
    }

      expandRequest <- expand.grid(links, mcYears)
      allGroup <- apply(expandRequest, 1, function(X){
        paste(timeStep, "data/links",paste(X, collapse = "/"),sep = "/")
      })
      if(showProgress){
        cat("Importing links\n")
      }
      linksData <- rbindlist(plyr::llply(1:length(allGroup), .readH5File, allGroup = allGroup,
                                         expandRequest = expandRequest,
                                         flags = flags,
                                         path = path,
                                         index = index
                                         , .progress = progressBar, .parallel = FALSE))

      ##Give names
      setnames(linksData, names(linksData), struct)
      tim <- getAllDateInfoFromDate(path, timeStep)

      #Add time
      linksData <- data.table(tim, linksData)
      # areasData

      #Add attribS
      linksData <- antaresRead:::.addClassAndAttributes(linksData,
                                                        synthesis,
                                                        attib$timeStep,
                                                        attib$opts,
                                                        simplify = TRUE, type = "links")

      if(mcYears[1] == "mcAll"){
        linksData[,mcYear := NULL]
      }

      linksData
  }



  #Districts


  #districts
  if(!is.null(districts)){
    if(districts == "all"){
      districts <- attib$opts$districtList


    }

    #Read structure
    H5close()
    struct <- h5readAttributes(path, paste0(timeStep, "/data/districts"))[[structName]]
    H5close()
    struct


    #No use select for districts (?antaresRead -> select)
    if(!select[1] == "all"){
      index <- c(1, which(struct[3:length(struct)]%in%select))
      struct <- struct[c(1:2, index+2)]
    }

    expandRequest <- expand.grid(districts, mcYears)
    allGroup <- apply(expandRequest, 1, function(X){
      paste(timeStep, "data/districts",paste(X, collapse = "/"),sep = "/")
    })
    if(showProgress){
    cat("Importing districts\n")
    }

    districtsData <- rbindlist(plyr::llply(1:length(allGroup), .readH5File, allGroup = allGroup,
                                       expandRequest = expandRequest,
                                       flags = flags,
                                       path = path,
                                       index = index
                                       , .progress = progressBar, .parallel = FALSE))

    ##Give names
    setnames(districtsData, names(districtsData), struct)
    tim <- getAllDateInfoFromDate(path, timeStep)

    #Add time
    districtsData <- data.table(tim, districtsData)
    # areasData

    #Add attribS
    districtsData <- antaresRead:::.addClassAndAttributes(districtsData,
                                                      synthesis,
                                                      attib$timeStep,
                                                      attib$opts,
                                                      simplify = TRUE, type = "districts")

    if(mcYears[1] == "mcAll"){
      districtsData[,mcYear := NULL]
    }

    districtsData
  }

  #Clusters
  if(!is.null(clusters)){
    if(clusters == "all"){
      clusters <- attib$opts$areasWithClusters

    }

    #Read structure
    H5close()
    struct <- h5readAttributes(path, paste0(timeStep, "/data/clusters"))[[structName]]
    H5close()



    #Open cluster names
    clusterName <- h5ls(H5Gopen(H5Fopen(path), paste0(timeStep, "/data/clusters/")),
         recursive = TRUE)

    clusterName <- clusterName$group[which(unlist(lapply(strsplit(clusterName$group, "/"), length)) >= 3)]
    clusterName <- unique(clusterName)
    clusterName <- gsub("^/", "", clusterName)

    #No use select for cluster (antaresRead)
    # if(!select[1] == "all"){
    #   index <- c(1, which(struct[4:length(struct)]%in%select))
    #   struct <- struct[c(1:3, index+3)]
    # }

    expandRequest <- expand.grid(clusterName, mcYears)
    allGroup <- apply(expandRequest, 1, function(X){
      paste(timeStep, "data/clusters",paste(X, collapse = "/"),sep = "/")
    })


    req <- Reduce(rbind, strsplit(as.character(expandRequest$Var1), "/"))
    req <- data.frame(req, row.names = NULL)

    expandRequest$Var1 <- NULL
    expandRequest <- cbind(req, expandRequest)
    if(showProgress){
    cat("Importing clusters\n")
    }
    clustersData <- rbindlist(plyr::llply(1:length(allGroup), .readH5File, allGroup = allGroup,
                                       expandRequest = expandRequest,
                                       flags = flags,
                                       path = path,
                                       index = index
                                       , .progress = progressBar, .parallel = FALSE))

    ##Give names
    setnames(clustersData, names(clustersData), struct)
    tim <- getAllDateInfoFromDate(path, timeStep)

    #Add time
    clustersData <- data.table(tim, clustersData)
    # areasData

    #Add attribS
    clustersData <- antaresRead:::.addClassAndAttributes(clustersData,
                                                      synthesis,
                                                      attib$timeStep,
                                                      attib$opts,
                                                      simplify = TRUE, type = "clusters")
    if(mcYears[1] == "mcAll"){
      clustersData[,mcYear := NULL]
    }

    clustersData
  }

  if((as.numeric(!is.null(clustersData)) +
      as.numeric(!is.null(linksData)) +
      as.numeric(!is.null(areasData))) >=2)
  {
    out <- list(areas = areasData,
         links = linksData,
         districts = districtsData,
         clusters = clustersData
         )
    out <- out[!unlist(lapply(out, is.null))]


    out <- antaresRead:::.addClassAndAttributes(out,
                                                         synthesis,
                                                         attib$timeStep,
                                                         attib$opts,
                                                         simplify = TRUE, type = NULL)
    out
  }else{
    if(!is.null(areasData)){
      return(areasData)
    }
    if(!is.null(linksData)){
      return(linksData)
    }
    if(!is.null(districtsData)){
      return(districtsData)
    }
    if(!is.null(clustersData)){
      return(clustersData)
    }
  }

}



