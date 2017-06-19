#' Transform a \code{antaresDataList} object to be writable by \link{writeAntaresH5}
#'
#' @param data \code{antaresDataList}
#' @param areasKey \code{character} organization key for areas, define h5 group and subgroup
#' @param linksKey \code{character} organization key for links, define h5 group and subgroup
#' @param clustersKey \code{character} organization key for clusters, define h5 group and subgroup
#'
transformH5 <- function(data,
                        areasKey = c("area"),
                        linksKey = c("link"),
                        clustersKey = c("area", "cluster")){

  if("areas"%in%names(data))
  {
    data$areas <- data$areas[, list(list(.SD)), by = areasKey]
  }
  if("links"%in%names(data))
  {
    data$links <- data$links[, list(list(.SD)), by = linksKey]
  }
  if("clusters"%in%names(data))
  {
    data$clusters <- data$clusters[, list(list(.SD)), by = clustersKey]
  }
  data
}


#' Write a h5 file from an object load with \link{readAntares}
#'
#' @param data \code{antaresDataList}
#' @param path \code{character} patch of h5 file
#' @param rootGroup \code{character} group will contain all h5 organization
#'
writeAntaresH5 <- function(data, path, rootGroup = NULL){
  H5close()
  sapply(names(data), function(X){
    tpData <- data[[X]]
    nameGroup <- paste(rootGroup, X, sep = "/")

    if(ncol(tpData)>1)
    {
      if(ncol(tpData)>2){
        groupData <- tpData[, .SD, .SDcols = 1:(ncol(tpData)-2)]
      }else{
        groupData <- data.frame()
      }

      creatGroup(nameGroup,
                groupData,
                 path)
    }
    sapply(1:nrow(tpData), function(Y){
      rowSel <- tpData[Y]
      nameGroup <- paste(rootGroup, X,

                         paste(unlist(rowSel[,lapply(.SD, as.character), .SDcols = 1:(ncol(rowSel)-1)]), collapse = "/"),

                         sep = "/")

      h5createDataset(path,
                      dim = dim(rowSel$V1[[1]]),
                      chunk=c(1,ncol(rowSel$V1[[1]])),
                      dataset = nameGroup)
      h5write(as.matrix(rowSel$V1[[1]]), path, nameGroup)
    })
  })
  TRUE
}


#' Create a group organization which subgroup.
#'
#' @param groupIn \code{character} name of main group (who will contain all organization)
#' @param groupData \code{data.table} organization
#' @param path \code{character} patch of h5 file
#'
creatGroup <- function(groupIn = NULL, groupData, path){
  if(!is.null(groupIn))
  {
    h5createGroup(path, groupIn)
  }

    if(ncol(groupData)>0){
      sapply(1:ncol(groupData), function(X){
        colummSel <- 1:X
        dataForGroupCreation <- groupData[, .SD, .SDcols = colummSel]
        dataForGroupCreation <- unique(dataForGroupCreation)
        dataForGroupCreation <- apply(dataForGroupCreation, 1, function(X){
          paste(X, sep = "/", collapse = "/")
        })
        dataForGroupCreation <- paste0(groupIn, "/", dataForGroupCreation)
        dataForGroupCreation <- unique(dataForGroupCreation)
        sapply(dataForGroupCreation, function(X){
          h5createGroup(path, X)

        })

      })
    }
}
