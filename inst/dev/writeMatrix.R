h5createFile("write_full_matrix.h5")
h5write(MAT, "write_full_matrix.h5","B")
H5close()
file.remove("write_full_matrix.h5")
