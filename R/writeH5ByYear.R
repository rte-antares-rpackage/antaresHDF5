#' Convert antares output to h5 file
#'
#' @param path \code{character} path of h5 file to write
#' @param timeSteps \code{character} timeSteps
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}. Defaut to \code{antaresRead::simOptions()}
#' @param writeMcAll \code{boolean} write mc-all
#'
#' @export
writeAntaresH5 <- function(path, timeSteps = c("hourly", "daily", "weekly", "monthly", "annual"),
                           opts = antaresRead::simOptions(),
                           writeMcAll = TRUE){
  # cl <- makeCluster(4)
  # clusterEvalQ(cl, {
  #   library(data.table)
  #   library(antaresRead)
  #   library(rhdf5)
  #   library(pipeR)
  #   library(stringr)
  #   library(ggplot2)
  #   library(antaresHdf5)
  #   library(parallel)
  #
  #
  # })
  # clusterExport(cl,c("path", "timeSteps", "opts", "writeMcAll"), envir = environment())
  fid <- H5Fopen(path)
  sapply(timeSteps, function(timeStep){

    allMcYears <- opts$mcYears
    if(writeMcAll){
      allMcYears <- c(allMcYears, -1)
    }

    print(allMcYears)
    sapply(allMcYears, function(mcY)
    {
      if(allMcYears[1] == mcY){
        writeStructure = TRUE
      }else{
        writeStructure = FALSE
      }
      writeMCallName <- FALSE
      if(mcY == -1){
        mcY <- NULL
        writeMCallName <- TRUE
      }

      res <- readAntares(areas = "all" ,
                         links = "all",
                         clusters = "all",
                         districts = "all",
                         mcYears = mcY,
                         timeStep = timeStep, opts = opts, showProgress = FALSE)

      if(writeStructure){

        attrib <- attributes(res)
        # Create group
        h5createGroup(path, timeStep)

        #Write time
        writeTime(res, path, timeStep)

        fid <- H5Fopen(path)
        #Write attributes
        writeAttribAndCreatGroup(path ,Y = attrib, timeStep)

      }

      #Remove useless data
      sapply(1:length(res), function(i){
        if("day" %in% names(res[[i]])){
          res[[i]][, day := NULL]
        }
        if("month" %in% names(res[[i]])){
          res[[i]][, month := NULL]
        }
        if("hour" %in% names(res[[i]])){
          res[[i]][, hour := NULL]
        }
        if("time" %in% names(res[[i]])){
          res[[i]][, time := NULL]
        }
      }) %>>% invisible()

      #Transform for write

      if(is.null(mcY)){

        lapply(res, function(X){
          X[, mcYear := "mcAll"]

        })
      }
      res <- transformH5(res,areasKey = c("area", "mcYear"),
                         linksKey = c("link",  "mcYear"),
                         districtKey = c("district",  "mcYear"),
                         clustersKey = c("area", "cluster",  "mcYear"))

      writeAntaresData(res, path, timeStep, writeStructure, writeMCallName)
    })
  })
}
