
#' Write a h5 file from an object load with \link{readAntares}
#'
#' @param data \code{antaresDataList}
#' @param path \code{character} patch of h5 file
#' @param rootGroup \code{character} group will contain all h5 organization
#'
#' @export
writeAntaresData <- function(data, path, rootGroup = NULL, writeStructure = TRUE){


  rootGroup <- paste0(rootGroup, "/data")
  if(writeStructure){
    H5close()
    h5createGroup(path, rootGroup)

  }




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

      if(writeStructure)
      {
        creatGroup(nameGroup,
                   groupData,
                   path)
      }
    }

    nams <- c(names(tpData[, .SD, .SDcols = 1:(ncol(tpData)-1)]),
              names(tpData$V1[[1]]))
    if(writeStructure)
    {
    h5writeAttribute(nams,
                     H5Gopen(H5Fopen(path),
                             nameGroup), "structure")
    }
    H5close()

    sapply(1:nrow(tpData), function(Y){
      rowSel <- tpData[Y]
      nameGroup <- paste(rootGroup, X,

                         paste(unlist(rowSel[,lapply(.SD, as.character), .SDcols = 1:(ncol(rowSel)-1)]), collapse = "/"),

                         sep = "/")

      h5createDataset(path,
                      dim = dim(rowSel$V1[[1]]),
                      chunk=c(nrow(rowSel$V1[[1]]),1),#ncol(rowSel$V1[[1]])
                      dataset = nameGroup, level = 1)


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
#' @export
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
