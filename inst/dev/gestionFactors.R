H5close()
file.remove("attributes.h5")
h5createFile("attributes.h5")

v <- c("A", "B", "A")
v <- as.factor(v)
attributes(v)

h5write(as.integer(v), "attributes.h5", "v")
h5writeAttribute(attr(v, "levels"), H5Dopen(H5Fopen("attributes.h5"), "v"), "levels")
h5writeAttribute(attr(v, "class"), H5Dopen(H5Fopen("attributes.h5"), "v"), "class")
H5close()

h5read("attributes.h5", "v")
h5read("attributes.h5", "v", read.attributes = T)


h5read("testWriteattrib.h5", "attributes/opts/energyCosts/spilled", read.attributes = TRUE)
h5read("testWriteattrib.h5", "attributes/opts/energyCosts", read.attributes = TRUE)
