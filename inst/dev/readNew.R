library(rhdf5)
library(data.table)
library(h5)
library(antaresHdf5)
library(antaresRead)


path <- "testNewFormat3.h5"




setSimulationPath("E:/DES/antaresFlowbased/TestrunSimulationFb/antaresStudy")
#setSimulationPath("C:/Users/titorobe/Desktop/test_case")


W <- readAntares(areas = "fr", mcYears = 1)
V <- h5ReadAntares(path, areas = "fr", mcYears = 1)

all(sapply(names(W), function(X){
  if(identical(V[[X]], W[[X]])){
    TRUE
  }else{
    identical(V[[X]], as.numeric(W[[X]]))
  }
}))


V$timeId[1:5]
W$timeId[1:5]
V$time- W$time




system.time(W <- readAntares(areas = "fr", mcYears = 1, select = "mustRun"))

names(W)

library(pipeR)
writeAntaresH5("toto.h5", compress = 1,  misc = TRUE,
               thermalAvailabilities = TRUE,
               hydroStorage = TRUE, hydroStorageMaxPower = TRUE, reserve = TRUE,
               linkCapacity = TRUE, mustRun = TRUE, thermalModulation = TRUE)



path <- "testNewFormat3.h5"
#path <- "toto.h5"
V <- h5ReadAntares(path,
                   clusters ="all",
                   mcYears = 1:100)




Bli2 <- h5ReadAntares(path, areas = "all")
h5ls("toto.h5")



V <- h5ReadAntares("testNewFormat3.h5",
                   clusters ="at", mcYears = 1, select = "production")
microbenchmark::microbenchmark(W <- readAntares(clusters = "de", mcYears = 1:10, select = "production"),
                               V <- h5ReadAntares("testNewFormat3.h5",
                                                  clusters ="de", mcYears = 1:10, select = "production"))

Rprof(tmp <- tempfile())
W <- h5ReadAntares("testNewFormat3.h5",
              clusters ="de", mcYears = 1, select = "production")
Rprof()
summaryRprof(tmp)

res <- h5ReadAntares(path = path, areas = "all", mcYears = c(6, 7, 9:12), select = "all")




W <- readAntares(areas = "fr", mcYears = 1)


path
areas = "fr"
links = NULL
clusters = NULL
districts = NULL
mcYears = 1
timeStep = "hourly"
select = "all"
showProgress = TRUE
simplify = TRUE
perf = TRUE








path <- "bigStud.h5"
res <- h5ReadAntares(path = path,
                    areas = "all",
                    links = "all",
                    districts = "all",
                    clusters = "all",
                    mcYears = 1)

res <- h5ReadAntares(path = path,
                     areas = "all",
                     links = "all",
                     districts = "all",
                     clusters = "all",
                     mcYears = NULL)

res <- h5ReadAntares(path = path,
                     areas = "fr",
                     links = "be - de",
                     districts = "fr",
                     clusters = "fr",
                     mcYears = NULL)

res <- h5ReadAntares(path = path,
                     areas = "all",
                     links = "all",
                     districts = "all",
                     clusters = "all",
                     mcYears = "all")



res <- h5ReadAntares(path = path,
                     areas = "all",
                     links = "all",
                     districts = "all",
                     clusters = "all",
                     mcYears = "all",timeStep = "daily")

res <- h5ReadAntares(path = path,
                     areas = "all",
                     links = "all",
                     districts = "all",
                     clusters = "all",
                     mcYears = "all",timeStep = "weekly")

res <- h5ReadAntares(path = path,
                     areas = "all",
                     links = "all",
                     districts = "all",
                     clusters = "all",
                     mcYears = "all",timeStep = "monthly")

res <- h5ReadAntares(path = path,
                     areas = "all",
                     links = "all",
                     districts = "all",
                     clusters = "all",
                     mcYears = "all",timeStep = "annual")



path
areas = "all"
links = NULL
clusters = NULL
districts = NULL
mcYears = 1
timeStep = "hourly"
select = "all"
showProgress = TRUE
simplify = TRUE
perf = TRUE


#'
#'
#'
#'
#'
#'
#'
#'
#' #' Read data parrallel test
#' #'
#' #' @param path {character} path of h5file to load
#' #' @param areas see \link[antaresRead]{readAntares}
#' #' @param links see \link[antaresRead]{readAntares}
#' #' @param clusters see \link[antaresRead]{readAntares}
#' #' @param districts see \link[antaresRead]{readAntares}
#' #' @param mcYears see \link[antaresRead]{readAntares}
#' #' @param timeStep see \link[antaresRead]{readAntares}
#' #' @param select see \link[antaresRead]{readAntares}
#' #' @param showProgress see \link[antaresRead]{readAntares}
#' #' @param simplify see \link[antaresRead]{readAntares}
#' #' @param perf \code{boolean}, eval performance during developpement time, to remove
#' #'
#' #' @export
#' writeAntaresH5Parrallel <- function(path, timeSteps = c("hourly", "daily", "weekly", "monthly", "annual"),
#'                                     opts = antaresRead::simOptions(),
#'                                     writeMcAll = TRUE,
#'                                     compress = 1,
#'                                     misc = FALSE,
#'                                     thermalAvailabilities = FALSE,
#'                                     hydroStorage = FALSE,
#'                                     hydroStorageMaxPower = FALSE,
#'                                     reserve = FALSE,
#'                                     linkCapacity = FALSE,
#'                                     mustRun = FALSE,
#'                                     thermalModulation = FALSE
#' ){
#'
#'   #Create h5 file
#'   h5createFile(path)
#'
#'
#'   cl <- makeCluster(5)
#'   clusterEvalQ(cl, {
#'     library(antaresHdf5)
#'     library(antaresRead)
#'     library(rhdf5)
#'     library(data.table)
#'     library(pipeR)
#'   })
#'   clusterExport(cl, c("opts", "writeMcAll", "compress", "misc", "thermalAvailabilities",
#'                       "hydroStorage", "hydroStorageMaxPower", "reserve", "linkCapacity",
#'                       "mustRun", "thermalModulation"), envir = environment())
#'
#'
#'
#'   #loop on timeStep
#'   parSapplyLB(cl, timeSteps, function(timeStep){
#'
#'     #Add mcAll
#'     allMcYears <- opts$mcYears
#'     if(writeMcAll){
#'       allMcYears <- c(allMcYears, -1)
#'     }
#'
#'     #Loop on MCyear
#'     sapply(allMcYears, function(mcY)
#'     {
#'       if(allMcYears[1] == mcY){
#'         writeStructure = TRUE
#'       }else{
#'         writeStructure = FALSE
#'       }
#'       mcAll <- FALSE
#'       if(mcY == -1){
#'         mcY <- NULL
#'         writeStructure <- TRUE
#'         mcAll <- TRUE
#'       }
#'
#'       #Read data
#'       res <- readAntares(areas = "all" ,
#'                          links = "all",
#'                          clusters = "all",
#'                          districts = "all",
#'                          mcYears = mcY,
#'                          timeStep = timeStep, opts = opts, showProgress = FALSE,
#'                          misc = misc, thermalAvailabilities = thermalAvailabilities,
#'                          hydroStorage = hydroStorage, hydroStorageMaxPower = hydroStorageMaxPower,
#'                          reserve = reserve, linkCapacity = linkCapacity, mustRun = mustRun,
#'                          thermalModulation = thermalModulation)
#'
#'       if(writeStructure & !mcAll){
#'
#'         attrib <- attributes(res)
#'         # Create group
#'         H5close()
#'         h5createGroup(path, timeStep)
#'         H5close()
#'         #Write time
#'         writeTime(res, path, timeStep)
#'         H5close()
#'         #Write attributes
#'         s <- serialize(attrib, NULL, ascii = TRUE)
#'         h5write(rawToChar(s), path, paste0(timeStep, "/attrib"))
#'       }
#'
#'       #Remove useless data
#'       sapply(1:length(res), function(i){
#'         if("day" %in% names(res[[i]])){
#'           res[[i]][, day := NULL]
#'         }
#'         if("month" %in% names(res[[i]])){
#'           res[[i]][, month := NULL]
#'         }
#'         if("hour" %in% names(res[[i]])){
#'           res[[i]][, hour := NULL]
#'         }
#'         if("time" %in% names(res[[i]])){
#'           res[[i]][, time := NULL]
#'         }
#'       }) %>>% invisible()
#'       gc()
#'
#'
#'       if(is.null(mcY)){
#'
#'         lapply(res, function(X){
#'           X[, mcYear := "mcAll"]
#'
#'         })
#'       }
#'       #Transform for write
#'       res <- transformH5(res,areasKey = c("area", "mcYear"),
#'                          linksKey = c("link",  "mcYear"),
#'                          districtKey = c("district",  "mcYear"),
#'                          clustersKey = c("area", "cluster",  "mcYear"))
#'       #Write data
#'       writeAntaresData(res, path, timeStep, writeStructure, mcAll, compress)
#'     })
#'   })
#' }
#'
