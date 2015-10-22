#' Create decomposition matrix
#'
#' @description Create a decomposition matrix from specified turnover and transfers.
#' @param par named array of turnover times (tau) and transfers (trans) following patterns specified in \code{publishedParameters.R}
#' @param tauStr string with the regexp that matches the name of the turnover times in par
#' @param transStr string with the regexp that matches the name of the transfer proportions in par
#' @param verbose boolean for debuging statements
#'
#' @return matrix representing the decomposition rate
#' @export
#' @import assertthat
#' 
#' @examples
#' makeDecompMatrix(unlist(list(tau1=10, tau2=100, A2=0.5)))
makeDecompMatrix <- function(par, tauStr='tau', transStr='A', verbose=FALSE){
  assert_that(!is.null(names(par)))
  assert_that(any(grepl('tau', names(par))))
  
  tau <- par[grepl('tau', names(par))]
  tau <- tau[as.numeric(gsub(tauStr, '', names(tau)))]
  assert_that(all(tau > 0))
  k <- diag(1/tau, nrow=length(tau))
  if(verbose) {cat('K:\n'); print(k)}
  
  trans <- par[grepl(transStr, names(par))]
  trans <- data.frame(name=names(trans),  value=trans)
  trans$index <- as.numeric(gsub(transStr, '', trans$name))
  
  a <- matrix(0, nrow=length(tau), ncol=length(tau))
  a[trans$index] <- trans$val
  if(verbose) {cat('A:\n'); print(a)}
  assert_that(all(diag(a) == 0))
  assert_that(all(colSums(a) < 1))
  diag(a) <- -1
  return(a%*%k)
}
