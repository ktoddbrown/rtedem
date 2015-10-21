#' dC/dt for first order linear decay model
#'
#' @description The rate change of a first order linear decay model.
#'
#' @param y array with current carbon stock
#' @param parms named vector with turnover times or a list of the format specified in \code{publishedParameters}
#'
#' @return rate change of first order linear decay model
#' @export
#' @import assertthat
dC.firstOrderModel <- function(t, y, parms, tauStr='tau', transStr='A', verbose=FALSE){
  if(verbose){cat('y:\n'); print(y)}
  Kmatrix <- makeDecompMatrix(parms, tauStr=tauStr, transStr=transStr)
  if(verbose){cat('K:\n'); print(Kmatrix)}
  assert_that(dim(Kmatrix)[1] == length(y))
  dim(y) <- c(dim(Kmatrix)[1], 1)
  if(verbose){cat('y:\n'); print(y)}
  return(list(Kmatrix %*% y))
}