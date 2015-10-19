#' Create decomposition matrix
#'
#' @description Create a decomposition matrix from specified turnover and transfer arrays.
#' @param par list of turnover times (tau) and transfers (trans) following patterns specified in \code{publishedParameters.R}
#'
#' @return a list of matricies
#' @export
#'
makeDecompMatrix <- function(par = publishedParameters()[['CENTURY']]){
  k <- diag(1/par$tau)
  a <- matrix(0, nrow=length(par$tau), ncol=length(par$tau))
  index <- as.numeric(gsub('A', '', par$trans$name))
  a[index] <- par$trans$val
  diag(a) <- -1
  return(a%*%k)
}
