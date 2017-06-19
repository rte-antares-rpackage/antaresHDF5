
H5close()
file.remove("attributes.h5")
h5createFile("attributes.h5")

h5createGroup("attributes.h5", "gr")
v <- c("A", "B", "A")
v <- as.factor(v)
attributes(v)

h5write(as.integer(v), "attributes.h5", "gr/v")
h5writeAttribute(attr(v, "levels"), H5Dopen(H5Fopen("attributes.h5"), "gr/v"), "levels")
h5writeAttribute(attr(v, "class"), H5Dopen(H5Fopen("attributes.h5"), "gr/v"), "class")
H5close()

h5read("attributes.h5", "gr/v")
h5read("attributes.h5", "gr/v", read.attributes = T)

h5read("attributes.h5", "gr")
h5read("attributes.h5", "gr", read.attributes = T)


h5dump(H5Gopen(H5Fopen("attributes.h5"), "gr"), all = T, read.attributes = TRUE)
