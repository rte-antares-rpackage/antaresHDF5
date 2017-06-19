h5createFile("write_full_datatable.h5")
h5write(DTAB, "write_full_datatable.h5","B")
H5close()
file.remove("write_full_datatable.h5")
