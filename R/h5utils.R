
.getTimStep <- function(fid){
  timeSteps <- sapply(c("hourly", "daily", "weekly", "monthly", "annual"), function(X){
    H5Lexists(fid, X)
  })
  names(timeSteps[which(timeSteps == TRUE)])
}
