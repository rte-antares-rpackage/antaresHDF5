#' Write list from attributes in h5.
#'
#' @param Y \code{list}, list of study attributes
#' @param group \code{character} group where attributes will be write
#' @param path \code{character} patch of h5 file
#'
#' @import pipeR stringr testthat
#' @export
writeAttribAndCreatGroup <- function(path, Y, group = NULL){

  group <- paste0(group, "/attributes")
  h5createGroup(path, group)
  fid <- H5Fopen(path)
  writeList(path, fid, Y, group)

}




#' Write list from attributes in h5.
#'
#' @param Y \code{list}, list of study attributes
#' @param group \code{character} group where attributes will be write
#' @param path \code{character} patch of h5 file
#' @param fid \code{numeric} id of h5 file
#'
writeList  <- function(path, fid, Y, group = NULL){
  sapply(names(Y), function(X, Y){
    #fid <- H5Fopen(path)
    nam <- X
    if(nam == ""){
      nam <- "Noname"
      X <- Y[[1]]
    }else{
      X <- Y[[X]]
    }

    isWrited <- FALSE
    if(class(X)[1] %in% c("list", "simOptions")){
      nam <- paste(group, nam, sep = "/")
      H5Gcreate(fid, nam)
      writeList(path, fid, X, group = nam)
    }else{
      classOrigin <- class(X)
      objectGroupName <- paste(group, nam, sep = "/")
      if(class(X)[1]  == "POSIXlt"){
        X <- as.numeric(X)
        X <- c("posictClass", X)
      }

      if(class(X)[1]  == "factor"){

        X <- as.character(X)
        # h5write(as.integer(X), path, "v")
        # h5writeAttribute(attr(X, "levels"), H5Dopen(H5Fopen(path), objectGroupName), "levels")
        # h5writeAttribute(attr(X, "class"), H5Dopen(H5Fopen(path), objectGroupName), "class")
        # H5close()
      }else if ( is.data.table(X)){
        colFactors <- names(which(lapply(X, class) == "factor"))
        if(length(colFactors)>0){
          X[,c(colFactors) := lapply(.SD, as.character), .SDcols = colFactors]
        }
        X <- data.frame(X)
      }else{
        if(!is.null(names(X))){
          #X <- data.table("Namvect", t(X))
          isWrited = TRUE
          if(length(X)>0)
          {
            if(is.null(dim(X)))
            {

              if(is.na(X[1])){
                X <- "NA"
              }
            }
          }
          h5write(X, path, paste0(objectGroupName), write.attributes = TRUE)
          h5writeAttribute(names(X), H5Dopen(fid, objectGroupName), "names")
          #H5close()
        }
      }
      if(!isWrited){
        if(length(X)>0)
        {
          if(is.null(dim(X)))
          {

            if(is.na(X[1])){
              X <- "NA"
            }

          }
        }


        if(class(X) == "data.frame")
        {
          h5write(X, path, paste0(objectGroupName), write.attributes = TRUE)
        }else{
        size <- length(X)
          tid <- switch(storage.mode(X)[1], double = rhdf5:::h5constants$H5T["H5T_NATIVE_DOUBLE"],
                        integer = rhdf5:::h5constants$H5T["H5T_NATIVE_INT32"],
                        logical = rhdf5:::h5constants$H5T["H5T_NATIVE_INT32"],
                        character = {
                          tid <- H5Tcopy("H5T_C_S1")
                          if (!is.numeric(size)) {
                            stop("parameter 'size' has to be defined for storage.mode character.")
                          }
                          H5Tset_size(tid, size)
                          tid
                        }, {
                          stop("datatype ", storage.mode, " not yet implemented. Try 'double', 'integer', or 'character'.")
                        })


        sid <- H5Screate_simple(size)
        did <- H5Dcreate(fid, objectGroupName, tid, sid)
        H5Dwrite(did, X, h5spaceMem = sid, h5spaceFile = sid)
        H5Dclose(did)
        H5Sclose(sid)

        #h5write(X, path, paste0(objectGroupName), write.attributes = TRUE)
        #H5close()
      }}
    }
  }, Y = Y)
  NULL
}


#' Rescover format after read attributes from h5 file.
#'
#' @param attribList \code{list}, list of study attributes read from h5.
#'
#'@export
giveFormat <- function(attribList){
  sapply(names(attribList) , function(X){
    Y <- attribList[[X]]
    if(is.list(Y) & ! is.data.frame(Y)){
      giveFormat(Y)
    }else{
      if(class(Y) == "array"){
        Y <-  c(Y)
        if(length(Y) > 1){
          if(Y[1] == "posictClass"){
            Y <- as.POSIXlt(as.numeric(Y[2]), origin = "1970-01-01", tz = "UTC")
          }
        }


        if(!"POSIXlt" %in%class(Y))
        {
          if(length(Y)>0)
          {
          if(Y[1] == "NA"){
            Y <- NA
          }
          }
        }

        Y
      }else if(is.data.frame(Y)){
        Y <- data.table::data.table(Y)

        # if(unlist(Y[1,.SD, .SDcols = 1]) == "Namvect"){
        #   Y <- unlist(Y[,.SD, .SDcols = 2:ncol(Y)])
        # }

        # colFactors <- names(which(lapply(Y, class) == "character"))
        # if(length(colFactors)>0){
        #
        #   Y[,c(colFactors) := lapply(.SD, as.factor), .SDcols = colFactors]
        # }
        Y

      }else{
        if(length(Y) > 1){

          if(Y[1] == "posictClass"){
            Y <- as.POSIXct(as.numeric(Y[2]), origin = "1970-01-01", tz = "UTC")
          }

        }
        if(length(Y)>0)
        {
        if(Y[1] == "NA"){
          Y <- NA
        }
        }

        Y
      }
    }
  }, simplify = FALSE)
}

