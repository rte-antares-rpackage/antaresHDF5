
writeList  <- function(Y, group = NULL){
  sapply(names(Y), function(X){
    nam <- X
    X <- Y[[X]]
    isWrited <- FALSE
    if(class(X)[1] %in% c("list", "simOptions")){
      nam <- paste(group, nam, sep = "/")
      h5createGroup("testWriteattrib.h5", nam)
      writeList(X, group = nam)
    }else{
      classOrigin <- class(X)
      objectGroupName <- paste(group, nam, sep = "/")
      if(class(X)[1]  == "POSIXlt"){
        X <- as.numeric(X)
        X <- c("posictClass", X)
      }

      if(class(X)[1]  == "factor"){

        X <- as.character(X)
        # h5write(as.integer(X), "testWriteattrib.h5", "v")
        # h5writeAttribute(attr(X, "levels"), H5Dopen(H5Fopen("testWriteattrib.h5"), objectGroupName), "levels")
        # h5writeAttribute(attr(X, "class"), H5Dopen(H5Fopen("testWriteattrib.h5"), objectGroupName), "class")
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
          if(is.null(dim(X)))
          {

            if(is.na(X[1])){
              X <- "NA"
            }
          }
          h5write(X, "testWriteattrib.h5", paste0(objectGroupName), write.attributes = TRUE)
          h5writeAttribute(names(X), H5Dopen(H5Fopen("testWriteattrib.h5"), objectGroupName), "names")
          H5close()
        }
      }
      if(!isWrited){

        if(is.null(dim(X)))
        {

          if(is.na(X[1])){
            X <- "NA"
          }

        }

        h5write(X, "testWriteattrib.h5", paste0(objectGroupName), write.attributes = TRUE)
        H5close()
      }
    }
  })
  NULL
}

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
          if(Y[1] == "NA"){
            print(Y)
            Y <- NA
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
        if(Y[1] == "NA"){
          Y <- NA
        }

        Y
      }
    }
  }, simplify = FALSE)
}


testIdentical <- function(X, Y){
  sapply(names(X), function(Z){
    T1 <- X[[Z]]
    T2 <- Y[[Z]]
    if(class(T1)[1] %in% c("list", "simOptions", "data.table")){
      testIdentical(T1, T2)
    }else{
      identical(T1, T2)
    }
  })
}
