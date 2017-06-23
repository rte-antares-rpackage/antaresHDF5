#' Convert antares output to h5 file
#'
#' @param path \code{character} path of h5 file to write
#' @param timeStep \code{character} timeStep
#'
#' @export
writeAntaresH5 <- function(path, timeStep = "hourly", opts = antaresRead::simOptions()){

  allMcYears <- opts$mcYears
  print(allMcYears)
  sapply(allMcYears, function(mcY)
  {
    if(allMcYears[1] == mcY){
      writeStructure = TRUE
    }else{
      writeStructure = FALSE
    }

    res <- readAntares(areas = "all" ,
                       links = "all",
                       clusters = "all",
                       districts = "all",
                       mcYears = mcY,
                       timeStep = timeStep, opts = opts)

    if(writeStructure){

      attrib <- attributes(res)
      # Create group
      h5createGroup(path, timeStep)

      #Write time
      writeTime(res, path, timeStep)

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
    res <- transformH5(res,areasKey = c("area", "mcYear"),
                       linksKey = c("link",  "mcYear"),
                       districtKey = c("district",  "mcYear"),
                       clustersKey = c("area", "cluster",  "mcYear"))

    writeAntaresData(res, path, timeStep, writeStructure)
  })

}
