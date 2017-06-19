require(data.table)
require(rhdf5)

?IDate

antares <- readRDS("D:\\Users\\benothie\\Documents\\Mes fichiers reçus\\ExempleDate.RDS")

antares[, .N, day]
sapply(antares, class)


datetime <- seq(as.POSIXct("2017-12-02", tz = "UTC"), as.POSIXct("2017-12-04", tz = "UTC"), by = "hour")

datetime <- seq(as.POSIXct("2017-12-02", tz = "UTC"), by = "hour", length.out = 100000)

dt_date <- data.table(IDateTime(datetime, tz =" UTC"))


H5close()
file.remove("date.h5")
h5createFile("date.h5")

# ecriture
h5write(dt_date, "date.h5", "date")

# recuperation
datetime_data <- data.table(h5read("date.h5", "date"))

# affectation des classes
class(datetime_data$idate) <- c("IDate", "Date")
class(datetime_data$itime) <- c("ITime")


# recuperation de la locale actuelle du pc
current_locale <- Sys.getlocale(category = "LC_TIME")
# mise en locale english pour le time (extraction des mois)
Sys.setlocale("LC_TIME", "C")

# calculs des variables

datetime_data[, c("time", "day", "month", "hour") := list(
  as.POSIXct(idate, time = itime, tz = "UTC"),
  mday(idate),
  as.factor(toupper(format(idate, format = "%b"))),
  as.factor(substring(as.character(itime), 1, 5)))]


# reaffectation de la locale utilisateur
Sys.setlocale("LC_TIME", current_locale)
sapply(datetime_data, class)
