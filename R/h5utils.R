
.getTimStep <- function(fid){
  timeSteps <- sapply(c("hourly", "daily", "weekly", "monthly", "annual"), function(X){
    H5Lexists(fid, X)
  })
  names(timeSteps[which(timeSteps == TRUE)])
}

.writeAttributes <- function(res = NULL, path = NULL, timeStep = "hourly", fid = NULL, attributes = NULL)
{
  if(is.null(attributes))
  {
  attrib <- attributes(res)
  }else{
    attrib <- attributes
  }
  s <- serialize(attrib, NULL, ascii = TRUE)
  if(!is.null(path))
  {
  h5write(rawToChar(s), path, paste0(timeStep, "/attrib"))
  }else{
    did <- H5Dopen(fid,  paste0(timeStep, "/attrib"))
    H5Dwrite(did, rawToChar(s))
  }
}