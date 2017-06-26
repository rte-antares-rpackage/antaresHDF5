# path = "bleble.h5"
# clusters = "all"
# areas = "all"
# links = "all"
# timeStep = "hourly"
# districts = "all"
# select = "all"
# mcYears = NULL


readAntaresH5 <- function(path, areas = NULL, links = NULL, clusters = NULL,
                          districts = NULL, mcYears = NULL,
                          timeStep = "hourly", select = "all"){


  if(is.null(areas) & is.null(links) & is.null(clusters) & is.null(districts)){
    areas <- "all"
  }


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
  if(mcYears == "all"){
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
    cat("Importing areas\n")
    areasData <- rbindlist(plyr::llply(1:length(allGroup), .readH5File, allGroup = allGroup,
    expandRequest = expandRequest,
    flags = flags,
    path = path,
    index = index
    , .progress = "text", .parallel = FALSE))


    ##Give names
    setnames(areasData, names(areasData), struct)
    tim <- getAllDateInfoFromDate(path, timeStep)

    #Add time
    areasData <- data.table(tim, areasData)
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
      cat("Importing links\n")
      linksData <- rbindlist(plyr::llply(1:length(allGroup), .readH5File, allGroup = allGroup,
                                         expandRequest = expandRequest,
                                         flags = flags,
                                         path = path,
                                         index = index
                                         , .progress = "text", .parallel = FALSE))

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


  #Links
  if(!is.null(districts)){
    if(districts == "all"){
      districts <- attib$opts$districtList


    }

    #Read structure
    H5close()
    struct <- h5readAttributes(path, paste0(timeStep, "/data/districts"))[[structName]]
    H5close()
    struct

    if(!select[1] == "all"){
      index <- c(1, which(struct[3:length(struct)]%in%select))
      struct <- struct[c(1:2, index+2)]
    }

    expandRequest <- expand.grid(districts, mcYears)
    allGroup <- apply(expandRequest, 1, function(X){
      paste(timeStep, "data/districts",paste(X, collapse = "/"),sep = "/")
    })
    cat("Importing districts\n")

    districtsData <- rbindlist(plyr::llply(1:length(allGroup), .readH5File, allGroup = allGroup,
                                       expandRequest = expandRequest,
                                       flags = flags,
                                       path = path,
                                       index = index
                                       , .progress = "text", .parallel = FALSE))

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


    if(!select[1] == "all"){
      index <- c(1, which(struct[3:length(struct)]%in%select))
      struct <- struct[c(1:2, index+2)]
    }

    expandRequest <- expand.grid(clusterName, mcYears)
    allGroup <- apply(expandRequest, 1, function(X){
      paste(timeStep, "data/clusters",paste(X, collapse = "/"),sep = "/")
    })


    req <- Reduce(rbind, strsplit(as.character(expandRequest$Var1), "/"))
    req <- data.frame(req, row.names = NULL)

    expandRequest$Var1 <- NULL
    expandRequest <- cbind(req, expandRequest)
    cat("Importing clusters\n")
    clustersData <- rbindlist(plyr::llply(1:length(allGroup), .readH5File, allGroup = allGroup,
                                       expandRequest = expandRequest,
                                       flags = flags,
                                       path = path,
                                       index = index
                                       , .progress = "text", .parallel = FALSE))

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



