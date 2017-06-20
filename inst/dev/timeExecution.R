time <- getTime(res)


time



dt_date <- data.table(IDateTime(datetime, tz =" UTC"))


H5close()
file.remove("date.h5")
h5createFile("date.h5")





h5write(dt_date, "date.h5", "date")
writeAntaresData(res2, "testWrite22.h5", "blo")

h5ls(file)

dates <- getAllDateInfoFromDate("date.h5", "date")

