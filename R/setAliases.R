.setAlliasH5 <- function(){
  sapply(names(pkgEnvAntareasH5$process), function(X){
    tpAlias <- pkgEnvAntareasH5$process[[X]]
    X <- paste0("Out_", X)
    sapply(names(tpAlias), function(Y){
      varAlias <- tpAlias[[Y]]
      setAlias(X, X, c(Y, varAlias))
    })
  })
}
