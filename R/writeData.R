
#' Write a h5 file from an object load with \link{readAntares}
#'
#' @param data \code{antaresDataList}
#' @param path \code{character} patch of h5 file
#' @param rootGroup \code{character} group will contain all h5 organization
#' @param writeStructure \code{boolean}, write group and subgroup (only for first MCyear)
#' @param writeMCallName \code{character}, write mc-all names
#'
#' @export
writeAntaresData <- function(data, path, rootGroup = NULL, writeStructure = TRUE, writeMCallName = FALSE){



  #Transdorm path to id
  fid <- H5Fopen(path)


  rootGroup <- paste0(rootGroup, "/data")
  if(writeStructure){

    H5Gcreate(fid, rootGroup)

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
                   fid)
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

    if(writeMCallName)
    {
      h5writeAttribute(nams,
                       H5Gopen(H5Fopen(path),
                               nameGroup), "structureMcall")
    }

    sapply(1:nrow(tpData), function(Y){
      rowSel <- tpData[Y]
      nameGroup <- paste(rootGroup, X,

                         paste(unlist(rowSel[,lapply(.SD, as.character), .SDcols = 1:(ncol(rowSel)-1)]), collapse = "/"),

                         sep = "/")

      # h5createDataset(path,
      #                 dim = dim(rowSel$V1[[1]]),
      #                 chunk=c(nrow(rowSel$V1[[1]]), 1),###ncol(rowSel$V1[[1]])
      #                 dataset = nameGroup, level = 7)
      #
      #
      # h5write(as.matrix(rowSel$V1[[1]]), path, nameGroup)
      #

      GP <- nameGroup


      sid <- H5Screate_simple(c(dim(rowSel$V1[[1]])))

      did <- H5Dcreate(fid, GP, rhdf5:::h5constants$H5T["H5T_NATIVE_DOUBLE"], sid)

      H5Dwrite(did, as.matrix(rowSel$V1[[1]]), h5spaceMem = sid, h5spaceFile = sid)
      H5Dclose(did)
      H5Sclose(sid)






    })
  })
  H5Fclose(fid)
  TRUE
}


#' Create a group organization which subgroup.
#'
#' @param groupIn \code{character} name of main group (who will contain all organization)
#' @param groupData \code{data.table} organization
#' @param fid \code{numeric} id of hdf5 file
#'
#' @export
creatGroup <- function(groupIn = NULL, groupData, fid){
  if(!is.null(groupIn))
  {
    H5Gcreate(fid, groupIn)
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
        H5Gcreate(fid, X)

      })

    })
  }
}
