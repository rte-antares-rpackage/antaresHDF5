library(antaresRead)
library(rhdf5)
library(data.table)

testRead <- function(path){
  H5close()
  cat(paste0("\nTemps de lecture 1 colonne 1 MCyear 1 area : ",
             as.numeric(system.time(arrayToData.table(h5read(path, "areas", index = list(NULL, 1, 1, 1)))))[3]))
  H5close()
  cat(paste0("\nTemps de lecture 1 colonne & 1 MCyear & toutes area : ",
             as.numeric(system.time(arrayToData.table(h5read(path, "areas", index = list(NULL, 1, NULL, 1)))))[3]))
  H5close()
  cat(paste0("\nTemps de lecture toutes colonne & 1 MCyear & toutes area : ",
             as.numeric(system.time(arrayToData.table(h5read(path, "areas", index = list(NULL, NULL, NULL, 1)))))[3]))
  H5close()
  cat(paste0("\nTemps de lecture Tout : ",
             as.numeric(system.time(arrayToData.table(h5read(path, "areas"))))[3]))
  H5close()
}






testRead2 <- function(path){
  H5close()
  cat(paste0("\nTemps de lecture 1 colonne 1 MCyear 1 area : ",
             as.numeric(system.time(arrayToData.table(h5read(path, "areas", index = list(NULL, 1, 1, 1)))))[3]))
  H5close()
  cat(paste0("\nTemps de lecture 1 colonne & 1 MCyear & toutes area : ",
             as.numeric(system.time(arrayToData.table(h5read(path, "areas", index = list(NULL, 1, NULL, 1)))))[3]))
  H5close()
  cat(paste0("\nTemps de lecture toutes colonne & 1 MCyear & toutes area : ",
             as.numeric(system.time(arrayToData.table(h5read(path, "areas", index = list(NULL, NULL, NULL, 1)))))[3]))
  H5close()
  cat(paste0("\nTemps de lecture 8 colonne & 15 MCyear & 8 area : ",
             as.numeric(system.time(arrayToData.table(h5read(path, "areas", index = list(NULL, 1:8, 1:8, 1:15)))))[3]))
  H5close()
  cat(paste0("\nTemps de lecture Tout : ",
             as.numeric(system.time(arrayToData.table(h5read(path, "areas"))))[3]))
  H5close()
}




arrayToData.table <- function(array, dim = 2)
{
  W <- alply(array, dim, function(X)unlist(X, use.names = FALSE))
  setattr(W, "names", paste("V", 1:dim(array)[dim], sep = ""))
  dimS <- 1:length(dim(array))
  dimNot <- dimS[-dim]
  setattr(W, "row.names", .set_row_names(prod(dim(array)[dimNot])))
  setattr(W, "class", c("data.table", "data.frame"))
  alloc.col(W)
}

library(antaresRead)

opts <- setSimulationPath("C:/Users/titorobe/Desktop/antaresStudy", 1)
readAntar <- system.time(are <- readAntares("all"))
readAntar

path <- "testWrite10Year.h5"
H5close()
file.remove(path)

system.time(writeH5ByYear("10mcyearsNoCompressNoChunk.h5", opts,
                          mcYears = 1:10, level = 0, chunk = FALSE))
library(plyr)
testRead("10mcyearsNoCompressNoChunk.h5")



system.time(writeH5ByYear("10mcyearsCompress1NoChunk.h5", opts,
                          mcYears = 1:10, level = 1, chunk = FALSE))
testRead("10mcyearsCompress1NoChunk.h5")


system.time(writeH5ByYear("10mcyearsCompress5NoChunk.h5", opts,
                          mcYears = 1:10, level = 5, chunk = FALSE))
testRead("10mcyearsCompress5NoChunk.h5")



system.time(writeH5ByYear("10mcyearsCompress9NoChunk.h5", opts,
                          mcYears = 1:10, level = 9, chunk = FALSE))


testRead("10mcyearsCompress9NoChunk.h5")




system.time(writeH5ByYear("10mcyearsCompress0Chunk.h5", opts,
                          mcYears = 1:10, level = 0, chunk = TRUE))

testRead("10mcyearsCompress0Chunk.h5")


system.time(writeH5ByYear("10mcyearsCompress1Chunk.h5", opts,
                          mcYears = 1:10, level = 1, chunk = TRUE))
testRead("10mcyearsCompress1Chunk.h5")

system.time(writeH5ByYear("10mcyearsCompress5Chunk.h5", opts,
                          mcYears = 1:10, level = 5, chunk = TRUE))
testRead("10mcyearsCompress5Chunk.h5")

system.time(writeH5ByYear("10mcyearsCompress9Chunk.h5", opts,
                          mcYears = 1:10, level = 9, chunk = TRUE))
testRead("10mcyearsCompress9Chunk.h5")





system.time(writeH5ByYear("30mcyearsNoCompressNoChunk.h5", opts,
                          mcYears = 1:30, level = 0, chunk = FALSE))
testRead2("30mcyearsNoCompressNoChunk.h5")



system.time(writeH5ByYear("30mcyearsCompres1sNoChunk.h5", opts,
                          mcYears = 1:30, level = 1, chunk = FALSE))
testRead2("30mcyearsCompres1sNoChunk.h5")


system.time(writeH5ByYear("30mcyearsNoCompressChunk.h5", opts,
                          mcYears = 1:30, level = 0, chunk = TRUE))
testRead2("30mcyearsNoCompressChunk.h5")


system.time(writeH5ByYear("30mcyearsCompres1sChunk.h5", opts,
                          mcYears = 1:30, level = 1, chunk = TRUE))
testRead2("30mcyearsCompres1sChunk.h5")


system.time(writeH5ByYear("100mcyearsNoCompressNoChunk.h5", opts,
                          mcYears = 1:100, level = 0, chunk = FALSE))
testRead2("100mcyearsNoCompressNoChunk.h5")



# system.time(writeH5ByYear("100mcyearsCompress1NoChunk.h5", opts,
#                           mcYears = 1:100, level = 1, chunk = FALSE))
# testRead2("100mcyearsCompress1NoChunk.h5")
#




system.time(writeH5ByYear("100mcyearsCompress1Chunk.h5", opts,
                          mcYears = 1:100, level = 1, chunk = TRUE))
testRead2("100mcyearsCompress1Chunk.h5")



writeH5ByYear <- function(path, opts, mcYears = NULL, level = 0, chunk = FALSE){
  if(is.null(mcYears)){
    mcYears <- opts$mcYears
  }
  h5createFile(path)

  sapply(mcYears, function(mcyear){
    are <- readAntares("all", mcYears =mcyear)
    SD <- 8:ncol(are)
    array <- array(NA, c(nrow(are)/length(unique(are$area)), length(SD), length(unique(are$area))))

    country <- unique(are$area)
    areMat <- as.matrix(are[, .SD, .SDcols = SD])
    for(i in 1:length(country))
    {
      array[,,i] <- areMat[which(are$area==country[i]),]
    }


    if(mcyear == mcYears[1]){
      if(chunk == FALSE){
        chunk <- c(dim(array), length(mcYears))
      }else{
        chunk <- c(dim(array)[1],1,1,1)
      }
    h5createDataset(path,"areas", level = level, dims = c(dim(array), length(mcYears)), chunk = chunk)
    H5close()
    }
    fid <- H5Fopen(path)
    h5writeDataset.array(array, fid, "areas", index = list(NULL, NULL, NULL, which(mcyear==mcYears)))
    H5close()
  })

}

