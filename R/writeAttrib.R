#' Write list from attributes in h5.
#'
#' @param Y \code{list}, list of study attributes
#' @param group \code{character} group where attributes will be write
#'
#' @export
writeAttribAndCreatGroup <- function(path, Y, group = NULL){

  group <- paste0(group, "/attributes")
  h5createGroup(path, group)
  writeList(path, Y, group)

}




#' Write list from attributes in h5.
#'
#' @param Y \code{list}, list of study attributes
#' @param group \code{character} group where attributes will be write
#'
writeList  <- function(path, Y, group = NULL){
  sapply(names(Y), function(X){
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
      h5createGroup(path, nam)
      writeList(path, X, group = nam)
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
          h5writeAttribute(names(X), H5Dopen(H5Fopen(path), objectGroupName), "names")
          H5close()
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

        h5write(X, path, paste0(objectGroupName), write.attributes = TRUE)
        H5close()
      }
    }
  })
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

