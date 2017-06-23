
readAntaresH5 <- function(path, areas = NULL, links = NULL, clusters = NULL,
                          districts = NULL, mcYears = NULL,
                          timeStep = "hourly", select = "all"){
  #Areas
  if(is.null(areas)){
    attibAreas <- h5dump(H5Gopen(H5Fopen(path), paste0(timeStep, "/attributes")),
                         all = T, read.attributes = TRUE)

    attibAreas <- giveFormat(attibAreas)
    class(attibAreas$opts) <- "simOptions"

    if(is.null(mcYears)){
      mcYears <- attibAreas$opts$mcYears
    }

    if(is.null(areas) || areas == "all"){
      areas <- attibAreas$opts$areaList
    }

    mcYears <- paste0(mcYears)

    expandRequest <- expand.grid(areas, mcYears)
    allGroup <- apply(expandRequest, 1, function(X){
      paste(timeStep, "data/areas",paste(X, collapse = "/"),sep = "/")
    })

    # library(parallel)
    # cl <- makeCluster(4)
    # clusterEvalQ(cl, {
    #   library(data.table)
    #   library(antaresRead)
    #   library(rhdf5)
    #   library(pipeR)
    #   library(stringr)
    #   library(ggplot2)
    #   library(antaresHdf5)
    #   setSimulationPath("D:/Users/titorobe/Desktop/antaresStudy", 1)
    #
    # })
    # clusterExport(cl, c("allGroup", "path", "re"))
    #
    #
    # res <- rbindlist(parSapplyLB(cl, allGroup, function(VV){
    #   h5read(path, VV) %>>% data.table() %>>% re ()
    # }, simplify = FALSE))

    # clos <- function(d){
    #   H5close()
    #   d
    # }
    # res <- rbindlist(sapply(allGroup, function(VV){
    #   H5Dread(H5Dopen(H5Fopen(path), VV), index=c(1,NULL)) %>>% data.table() %>>% clos ()
    #  }, simplify = FALSE))
    # ##Sans parralelisation 2.84 secondes
    # ##Avec parralelisation 4 coeurs 2.42 secondes
    # h5read(path, VV, index = list(NULL, 1))

    H5close()
    struct <- h5readAttributes(path, paste0(timeStep, "/data/areas"))$structure
    H5close()
    struct

    if(!select[1] == "all"){
      index <- c(1, which(struct[3:length(struct)]%in%select))
      struct <- struct[c(1:2, index+2)]
    }else{
      index <- NULL
    }

    flags = h5default("H5F_ACC_RD")
    flags = rhdf5:::h5checkConstants("H5F_ACC_RD", flags)



    #Essai parallel, non concluant
    # nodes <- detectCores() - 1
    # cl <- makeCluster(nodes)
    # registerDoParallel(cl)



    data <- rbindlist(plyr::llply(1:length(allGroup), function(VV,
                                                               allGroup,
                                                               expandRequest,
                                                               flags,
                                                               path,
                                                               index
                                                               ){
      library(rhdf5)
      library(data.table)
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
    }, allGroup = allGroup,
    expandRequest = expandRequest,
    flags = flags,
    path = path,
    index = index
    , .progress = "text", .parallel = FALSE))


    setnames(data, names(data), struct)
    tim <- getAllDateInfoFromDate(path, timeStep)
    data <- data.table(tim, data)
    # setattr(data, "class", c("antaresDataTable", "antaresData", "data.table", "data.frame"))
    # sapply(names(attibAreas), function(X){
    #   setattr(data, X, attibAreas[[X]])
    # })
    # data
    data <- antaresRead:::.addClassAndAttributes(data,
                                                 attibAreas$synthesis,
                                                 attibAreas$timeStep,
                                                 attibAreas$opts,
                                                 simplify = TRUE, type = "areas")
    data

  }
}



