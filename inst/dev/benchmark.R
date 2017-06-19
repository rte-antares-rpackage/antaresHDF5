library(rhdf5)
library(antaresRead)
library(data.table)
library(microbenchmark)


setSimulationPath("D:/exemple_test", 1)
res <- readAntares(areas = "all", links = "all", clusters = "all", districts = "all", mcYears = "all")
#
# saveRDS(res$areas[c(1:10)], "ExempleDate.RDS")
#

DTAB <- data.frame(res$areas[, .SD, .SDcols = 7:ncol(res$areas)])
MAT <- as.matrix(res$areas[, .SD, .SDcols = 7:ncol(res$areas)])

#Ecriture data.frame VS ecriture martice dans le cas ou uniquement numeriques
microbenchmark(source("inst/dev/writeDataFrame.R"),
               source("inst/dev/writeMatrix.R"))

h5errorHandling(type="suppress")
#Mélange character et numerique
DTAB <- data.frame(res$areas)
MAT <- as.matrix(res$areas)
MAT[,1] <- as.numeric(as.factor(MAT[,1] ))
MAT[,3] <- as.numeric(as.factor(MAT[,3] ))
MAT[,5] <- as.numeric(as.factor(MAT[,5] ))
MAT[,6] <- as.numeric(as.factor(MAT[,6] ))
MAT <- apply(MAT, 2, as.numeric)


chaR <- data.frame(res$areas)[, c(1,3,5,6)]
nuM <- apply(as.matrix(res$areas)[, -c(1,3,5,6)],2,as.numeric)

#Cas 1 : écriture data.frame avec mélange char et num
#Cas 2 : écriture matrice avec uniquement numerique (besoin de table de lien pour les variables facteurs)
#Cas 3 : écriture de deux table 1, data.frame avec les colonnes charactère, table 2 matrice avec les colonnes numériques
microbenchmark(source("inst/dev/writeDataFrame.R"),
               source("inst/dev/writeMatrix.R"),
               source("inst/dev/writeDataframeForCharAndMatriceForNum.R")
               )

#Cas 1 rejeté
#Hésisation entre cas 2 et cas 3, le cas 2 est plus complexe à mettre en place (table de lien)
#pour des perfs casiment similaires


###Utilisation des chunks
#Sans chunk
h5createFile("testNoChunk.h5")
h5write(MAT, "testNoChunk.h5","B")
H5close()

#Avec chunk
h5createFile("testChunk.h5")
h5createDataset("testChunk.h5", "B",dim = dim(MAT),chunk=c(nrow(MAT),1))
h5write(MAT, "testChunk.h5","B")
H5close()


microbenchmark(h5read("testNoChunk.h5", "B", index=list(NULL, 30)),
h5read("testChunk.h5", "B", index=list(NULL, 30)))

#Préconisation : Pour un requêtage colonne faire des chunks


file.remove("testNoChunk.h5")
file.remove("testChunk.h5")



