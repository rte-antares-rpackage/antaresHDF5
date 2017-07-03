library(antaresRead)
# path = "bigStud.h5"
# clusters = "all"
# areas = "all"
# links = "all"
# timeStep = "hourly"
# districts = "all"
# select = "all"
# mcYears = NULL
#
#  areas = "all"
#  clusters = "all"
#  links = "all"
#  mcYears = 1
#  select = c("OV. COST", "OP. COST")

testIdentical <- function(X, Y){
  sapply(names(X), function(Z){
    T1 <- X[[Z]]
    T2 <- Y[[Z]]
    if(class(T1)[1] %in% c("list", "simOptions", "data.table")){
      testIdentical(T1, T2)
    }else{
      if(class(T1)[1] == "factor" |  class(T2)[1] == "factor"){
        T1 <- as.character(T1)
        T2 <- as.character(T2)
        identical(T1, T2)
      }else{
        identical(T1, T2)
      }

    }
  })
}



setSimulationPath("D:/Users/titorobe/Desktop/test_case", 1)


VV <- readAntares(areas = "all", links = "all", clusters = "all", districts = "all",  mcYears = 1:2, timeStep = "hourly")
WW <- readAntaresH5("smallStud.h5", areas = "all", links = "all", clusters = "all", districts = "all",  mcYears = 1:2, timeStep = "hourly")





attri <- testIdentical(attributes(VV), attributes(WW))
length(unlist(attri)[!unlist(attri)]) == 0


sapply(names(VV), function(Y)
{
  if(identical(sort(names(VV[[Y]])),  sort(names(WW[[Y]])))){
    all(sapply(names(VV[[Y]]), function(X){
      ifelse(identical(VV[[Y]][[X]], WW[[Y]][[X]]), TRUE,
             if(is.character(WW[[Y]][[X]]) | is.factor(WW[[Y]][[X]])){
               identical(as.character(VV[[Y]][[X]]),as.character(WW[[Y]][[X]]))
             }else{
               identical(as.integer(VV[[Y]][[X]]),as.integer(WW[[Y]][[X]]))
             })
    }))
  }else{
    FALSE
  }
})


