#' Recursively rbind a named list
#'
#' @param Y \code{list}, list
#' @param struct \code{character}, variables names
#'
#' @export
cbindRecusive <- function(Y, struct){
cbindRecu <- function(Y){
  rbindlist(sapply(names(Y), function(X){

    Z <- Y[[X]]
    if(is.list(Z)){
      names(Z) <- paste(names(Y[X]), names(Z), sep = "@@@@@")
      cbindRecu(Z)
    }else{
      data.table(X, Z)
    }
  }, simplify = FALSE))
}
Y <- cbindRecu(Y)
Y <- data.table(str_split(Y$X, "@@@@@", simplify = TRUE), Y[, .SD, .SDcols = 2:ncol(Y)])
names(Y)[1:length(struct)] <- struct
Y
}
