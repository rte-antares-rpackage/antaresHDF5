
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
