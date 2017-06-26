#' Get timeId from antares study
#'
#' @param data \code{antaresDataList} see \link{readAntares}
#' @param timeStep \code{character} timeStep
#'
#' @export
getTime <- function(data, timeStep){
  time <- unique(data[[1]]$time)
  current_locale <- Sys.getlocale(category = "LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  if(timeStep %in% c("weekly", "monthly")){
    dt_date <- data.table(time = as.character(time))

  }else if(timeStep == "annual"){
    dt_date <- data.table(time)

  }else{
    dt_date <- data.table(IDateTime(time, tz =" UTC"))

  }
  Sys.setlocale("LC_TIME", current_locale)
  dt_date
}

#' Read time and generate column who can be calculate from time
#'
#' @param path \code{character} path of h5 file
#' @param group \code{group} group where time are stocked
#'
#' @export
getAllDateInfoFromDate <- function(path, group){
  # affectation des classes

  groupT <- paste0(group, "/time")
  datetime_data <- data.table(h5read(path, groupT))
  if(group %in% c("weekly", "annual")){
    return(datetime_data)
  }

  current_locale <- Sys.getlocale(category = "LC_TIME")
  # mise en locale english pour le time (extraction des mois)
  Sys.setlocale("LC_TIME", "C")
  if(group == c("monthly")){


    timCop <- datetime_data$time
    timCop <- paste0(timCop, "-01")
    timCop <- as.Date(timCop)
    class(timCop) <- c("IDate", "Date")
    datetime_data$month <- as.factor(toupper(format(timCop, format = "%b")))

    return(datetime_data)
  }


  H5close()
  class(datetime_data$idate) <- c("IDate", "Date")
  class(datetime_data$itime) <- c("ITime")
  # recuperation de la locale actuelle du pc

  # calculs des variables
  datetime_data[, c("time", "day", "month") := list(
    as.POSIXct(idate, time = itime, tz = "UTC"),
    mday(idate),
    as.factor(toupper(format(idate, format = "%b")))
  )]

  if(group == "daily"){
    datetime_data$time <- as.Date(datetime_data$time)

  }
  mthList <- c("APR",
               "AUG",
               "DEC",
               "FEB",
               "JAN",
               "JUL",
               "JUN",
               "MAR",
               "MAY",
               "NOV",
               "OCT",
               "SEP")

  toAdd <- mthList[!mthList %in% levels(datetime_data$month)]
  if(length(toAdd)>0)
  {
    levels(datetime_data$month) <- c(levels(datetime_data$month), toAdd)
  }

  if(group == "hourly")
  {
    datetime_data[, c("hour") := as.factor(substring(as.character(itime), 1, 5))]
  }

  datetime_data[, idate := NULL]
  datetime_data[, itime := NULL]
  Sys.setlocale("LC_TIME", current_locale)
  datetime_data
}


#' Write time in h5 file
#'
#' @param data \code{antaresDataList} see \link{readAntares}
#' @param path \code{character} path of h5 file
#' @param group \code{group} group where time are stocked
#'
#' @export
writeTime <- function(data, path, group){
  time <- getTime(data, group)
  group <- paste0(group, "/time")
  h5write(time, path, group)
  H5close()

}
