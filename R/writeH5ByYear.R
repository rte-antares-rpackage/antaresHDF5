#' Convert antares output to h5 file
#'
#' @param path \code{character} path of h5 file to write
#' @param timeSteps \code{character} timeSteps
#' @param opts \code{list} of simulation parameters returned by the function \link{setSimulationPath}. Defaut to \code{antaresRead::simOptions()}
#' @param writeMcAll \code{boolean} write mc-all
#' @param compress \code{numeric} compress level
#' @param misc \code{boolean} see \link[antaresRead]{readAntares}
#' @param thermalAvailabilities \code{boolean} see \link[antaresRead]{readAntares}
#' @param hydroStorage \code{boolean} see \link[antaresRead]{readAntares}
#' @param hydroStorageMaxPower \code{boolean} see \link[antaresRead]{readAntares}
#' @param reserve \code{boolean} see \link[antaresRead]{readAntares}
#' @param linkCapacity \code{boolean} see \link[antaresRead]{readAntares}
#' @param mustRun \code{boolean} see \link[antaresRead]{readAntares}
#' @param thermalModulation \code{boolean} see \link[antaresRead]{readAntares}
#'
#'
#' @export
writeAntaresH5 <- function(path, timeSteps = c("hourly", "daily", "weekly", "monthly", "annual"),
                           opts = antaresRead::simOptions(),
                           writeMcAll = TRUE,
                           compress = 1,
                           misc = FALSE,
                           thermalAvailabilities = FALSE,
                           hydroStorage = FALSE,
                           hydroStorageMaxPower = FALSE,
                           reserve = FALSE,
                           linkCapacity = FALSE,
                           mustRun = FALSE,
                           thermalModulation = FALSE
                           ){

  #Create h5 file
  h5createFile(path)

  #loop on timeStep
  sapply(timeSteps, function(timeStep){

    #Add mcAll
    allMcYears <- opts$mcYears
    if(writeMcAll){
      allMcYears <- c(allMcYears, -1)
    }

    #Loop on MCyear
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

      #Read data
      res <- readAntares(areas = "all" ,
                         links = "all",
                         clusters = "all",
                         districts = "all",
                         mcYears = mcY,
                         timeStep = timeStep, opts = opts, showProgress = FALSE,
                         misc = misc, thermalAvailabilities = thermalAvailabilities,
                         hydroStorage = hydroStorage, hydroStorageMaxPower = hydroStorageMaxPower,
                         reserve = reserve, linkCapacity = linkCapacity, mustRun = mustRun,
                         thermalModulation = thermalModulation)

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


      if(is.null(mcY)){

        lapply(res, function(X){
          X[, mcYear := "mcAll"]

        })
      }
      #Transform for write
      res <- transformH5(res,areasKey = c("area", "mcYear"),
                         linksKey = c("link",  "mcYear"),
                         districtKey = c("district",  "mcYear"),
                         clustersKey = c("area", "cluster",  "mcYear"))
      #Write data
      writeAntaresData(res, path, timeStep, writeStructure, mcAll, compress)
    })
  })
  H5close()
  cat(paste0("File write : ", path))
  invisible()
}
