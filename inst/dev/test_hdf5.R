library(rhdf5)

v <- rnorm(100000000)

h5createFile("write_full_double.h5")
h5write(v, "write_full_double.h5","B")
h5ls("write_full_double.h5")

file.info("write_full_double.h5")
v2 <- h5read("write_full_double.h5","B")
head(v2)

h5createFile("write_full_double_round3.h5")
h5write(round(v, 3), "write_full_double_round3.h5","B")
h5ls("write_full_double_round3.h5")

file.info("write_full_double_round3.h5")
v2 <- h5read("write_full_double_round3.h5","B")
head(v2)

h5createFile("write_full_double_round2.h5")
h5write(round(v, 2), "write_full_double_round2.h5","B")
h5ls("write_full_double_round2.h5")

v2 <- h5read("write_full_double_round2.h5","B")
head(v2)

h5createFile("write_full_double_round1.h5")
h5write(round(v, 1), "write_full_double_round1.h5","B")
h5ls("write_full_double_round1.h5")

v2 <- h5read("write_full_double_round1.h5","B")
head(v2)

h5createFile("write_full_2decimal100.h5")
h5write(as.integer(round(v, 2)*100), "write_full_2decimal100.h5","B")
h5ls("write_full_2decimal100.h5")

v2 <- h5read("write_full_2decimal100.h5","B")
head(v2)

h5createFile("write_full_1decimal10.h5")
h5write(as.integer(round(v, 1)*10), "write_full_1decimal10.h5","B")
h5ls("write_full_1decimal10.h5")

v2 <- h5read("write_full_1decimal10.h5","B")
head(v2)

C = matrix(paste(LETTERS[1:10],LETTERS[11:20], collapse=""),
           nr=2,nc=5)
h5write(C, "myhdf5file.h5","foo/foobaa/C")
df = data.frame(1L:5L,seq(0,1,length.out=5),
                c("ab","cde","fghi","a","s"), stringsAsFactors=FALSE)
h5write(df, "myhdf5file.h5","df")
h5ls("myhdf5file.h5")


h5ls("/home/rte_tst/data/hdf5_2/2013_9.h5")


rhdf5::H5close()

h5createFile("subset_data_frame.h5")

d <- data.frame(x = rnorm(100), y= c("Apolp", "oinif"), stringsAsFactors = FALSE)

h5writeDataset(d, "subset_data_frame.h5","d3", DataFrameAsCompound = T)
h5ls("subset_data_frame.h5")

h5read("subset_data_frame.h5", "d")

h5

#-----------------------
# matrix subset
#-----------------------
d <- matrix(rnorm(10000000), ncol = 10)

h5createFile("ex_createDataset.h5")
h5write(d, "ex_createDataset.h5","A", level = 5)



# # create dataset with row paquets
# h5createDataset("ex_createDataset.h5", "B", dim(d), storage.mode = storage.mode(d), chunk=c(1000,1), level=7)
# system.time(h5write(d, file="ex_createDataset.h5", name="B"))

# user  system elapsed 
# 1.972   1.932   3.937 

# create dataset with compression, column  chunk
h5createDataset("ex_createDataset.h5", "C", dim(d), storage.mode = storage.mode(d), chunk=c(nrow(d),1), level=7)
system.time(h5write(d, file="ex_createDataset.h5", name="C"))
# user  system elapsed 
# 3.536   2.316   5.863 

# create dataset with compression, row  chunk
h5createDataset("ex_createDataset.h5", "D", dim(d), storage.mode = storage.mode(d), chunk=c(1,ncol(d)), level=7)
system.time(h5write(d, file="ex_createDataset.h5", name="D"))

h5createAttribute(fid, "rownames", nrow(obj), storage.mode="character",
                  size=max(nchar(rownames(obj))))
h5createAttribute(fid, "colnames", ncol(obj), storage.mode="character",
                  size=max(nchar(colnames(obj))))

h5writeDataset(obj, fid, name)
h5writeAttribute(rownames(obj), did, "rownames")
h5writeAttribute(colnames(obj), did, "colnames")


# create dataset with compression, row  chunk
h5createDataset("ex_createDataset.h5", "F", dim(d), storage.mode = storage.mode(d), chunk=c(10000,5), level=7)
system.time(h5write(d, file="ex_createDataset.h5", name="F"))

# user  system elapsed 
# 35.972  19.600  50.219 


system.time(read_all <- h5read("ex_createDataset.h5","A"))
system.time(read_all2 <- h5read("ex_createDataset.h5","B"))
system.time(read_all3 <- h5read("ex_createDataset.h5","C"))
# system.time(read_all4 <- h5read("ex_createDataset.h5","D"))
system.time(read_all4 <- h5read("ex_createDataset.h5","F"))

dim(read_all)
head(read_all)

# subset on row
system.time(read_sr <- h5read("ex_createDataset.h5","A", index = list(seq(1, 1000000, by = 200), NULL)))
system.time(read_sr <- h5read("ex_createDataset.h5","B", index = list(seq(1, 1000000, by = 200), NULL)))
system.time(read_sr <- h5read("ex_createDataset.h5","C", index = list(seq(1, 1000000, by = 200), NULL)))
# system.time(read_sr <- h5read("ex_createDataset.h5","D", index = list(seq(1, 1000000, by = 500), NULL)))
system.time(read_sr <- h5read("ex_createDataset.h5","F", index = list(seq(1, 1000000, by = 200), NULL)))

# subset on column
system.time(read_sc <- h5read("ex_createDataset.h5","A", index = list(NULL, c(1,3,5,9))))
system.time(read_sc <- h5read("ex_createDataset.h5","B", index = list(NULL, c(1,3,5,9))))
system.time(read_sc <- h5read("ex_createDataset.h5","C", index = list(NULL, c(1,3,5,9))))
# system.time(read_sc <- h5read("ex_createDataset.h5","D", index = list(NULL, c(1,3,5))))
system.time(read_sc <- h5read("ex_createDataset.h5","F", index = list(NULL, c(1,3,5,9))))

# subset on column
system.time(read_sc <- h5read("ex_createDataset.h5","A", index = list(NULL, c(1,3))))
system.time(read_sc <- h5read("ex_createDataset.h5","B", index = list(NULL, c(1,3))))
system.time(read_sc <- h5read("ex_createDataset.h5","C", index = list(NULL, c(1,3))))
# system.time(read_sc <- h5read("ex_createDataset.h5","D", index = list(NULL, c(1,3,5))))
system.time(read_sc <- h5read("ex_createDataset.h5","F", index = list(NULL, c(1,3))))

# subset on row & column
system.time(read_sc <- h5read("ex_createDataset.h5","A", index = list(seq(1, 1000000, by = 500), c(1,3,5))))
system.time(read_sc <- h5read("ex_createDataset.h5","B", index = list(seq(1, 1000000, by = 500), c(1,3,5))))
system.time(read_sc <- h5read("ex_createDataset.h5","C", index = list(seq(1, 1000000, by = 500), c(1,3,5))))
system.time(read_sc <- h5read("ex_createDataset.h5","D", index = list(seq(1, 1000000, by = 500), c(1,3,5))))

dim(read_sr)
head(read_sr)

h5ls("ex_createDataset.h5")


#-----------------------
# data.frame subset

#-----------------------
d <- data.frame(matrix(rnorm(10000000), ncol = 10))
h5createFile("ex_df.h5")
system.time(h5write(d, "ex_df.h5","A", level = 5))


d <- matrix(rnorm(10000000), ncol = 10)
colnames(d) <- paste0("X", 1:10)
h5createFile("ex_mat.h5")
system.time(h5write(d, "ex_mat.h5","A", level = 5))

did <- H5Dopen("ex_mat.h5","A")

h5writeAttribute(colnames(d), H, "rownames")


# create dataset with row paquets
tt <- h5read("ex_mat.h5","A", read.attributes=TRUE)
head(tt)

system.time(d2 <- h5read("ex_df.h5","A"))
head(d2)

system.time(d3 <- h5read("ex_mat.h5","A"))
head(d3)


h5createFile("ex_ff.h5")

h5write(c("a", "b"), "ex_ff.h5","A", level = 5)
h5read("ex_ff.h5","A")
