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
                           writeMcAll = TRUE,
                           compress = 0){
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
 # fid <- H5Fopen(path)
  # clusterExport(cl,c("path", "timeSteps", "opts", "writeMcAll", "fid"), envir = environment())
  h5createFile(path)

  sapply(timeSteps, function(timeStep){
    allMcYears <- opts$mcYears
    if(writeMcAll){
      allMcYears <- c(allMcYears, -1)
    }

    sapply(allMcYears, function(mcY)
    {
      if(allMcYears[1] == mcY){
        writeStructure = TRUE
      }else{
        writeStructure = FALSE
      }
      mcAll <- FALSE
      if(mcY == -1){
        mcY <- NULL
        writeStructure <- TRUE
        mcAll <- TRUE
      }

      res <- readAntares(areas = "all" ,
                         links = "all",
                         clusters = "all",
                         districts = "all",
                         mcYears = mcY,
                         timeStep = timeStep, opts = opts, showProgress = FALSE)

      if(writeStructure & !mcAll){

        attrib <- attributes(res)
        # Create group
        H5close()
        h5createGroup(path, timeStep)
        H5close()
        #Write time
        writeTime(res, path, timeStep)
        H5close()
        #Write attributes
        # writeAttribAndCreatGroup(path ,Y = attrib, timeStep)
        #New attrib write (convert to bin)
        s <- serialize(attrib, NULL, ascii = TRUE)
        h5write(rawToChar(s), path, paste0(timeStep, "/attrib"))
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
      gc()
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

      writeAntaresData(res, path, timeStep, writeStructure, mcAll, compress)
    })
  })
}
