# Fast data.table affect
#' @export
#'
as.data.table.matrix.fast <- function (x, ...)
{
  d <- dim(x)
  nrows <- d[1L]
  ir <- seq_len(nrows)
  ncols <- d[2L]
  ic <- seq_len(ncols)
  dn <- dimnames(x)
  value <- lapply(ic, function(i) x[,i])
  setattr(value, "names", paste("V", ic, sep = ""))
  setattr(value, "row.names", .set_row_names(nrows))
  setattr(value, "class", c("data.table", "data.frame"))
  alloc.col(value)
  #value
}

#' # Fast data.table affect
#' #' @export
#' #'
#' as.data.table.matrix.fast2 <- function (x, ...)
#' {
#'
#'   d <- dim(x)
#'   nrows <- d[1L]
#'   ir <- seq_len(nrows)
#'   ncols <- d[2L]
#'   ic <- seq_len(ncols)
#'   dn <- dimnames(x)
#'   value <- vector("list", ncols)
#'   for (i in ic) value[[i]] <- x[, i]
#'   setattr(value, "names", paste("V", ic, sep = ""))
#'   setattr(value, "row.names", .set_row_names(nrows))
#'   setattr(value, "class", c("data.table", "data.frame"))
#'   #alloc.col(value)
#'   value
#' }
