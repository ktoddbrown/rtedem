#' dC/dt for first order linear decay model
#'
#' @description The rate change of a first order linear decay model.
#'
#' @param C array with current carbon stock
#' @param par named vector with turnover times or a list of the format specified in \code{publishedParameters}
#'
#' @return rate change of first order linear decay model
#' @export
#' @import assertthat
dC.firstOrderModel <- function(C, par){
  assert_that(any(grepl('tau', names(par))))
  if(class(par$tau) %in% 'list'){
    Kmatrix <- makeDecompMatrix(modelBase=par)[[1]]
  }else{
    Kmatrix <- makeDecompMatrix(modelBase=list(model1=list(tau=par[grepl('tau',names(par))], trans=data.frame(name=names(par)[!grepl('tau',names(par))], val=par[!grepl('tau',names(par))]))))[[1]]
  }
  dim(C) <- c(1, dim(Kmatrix)[1])
  return(Kmatrix %*% C)
}