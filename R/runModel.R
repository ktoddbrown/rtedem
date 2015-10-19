#' Run a decomposition model
#'
#' @description Run a model across several different cases defined by different applocation parameters but which share the same decomposition and transfer parameters.
#' 
#' @param par a named numeric array. The allocation parameters end with 'aD' where D is an integer identifying the associated pool, and the pool name starts with 'lableN.' where lableN is the case lable. The turnover parameters are named 'turnoverD' where D is an integer identifying the associated pool. The transfer parameters are named 'transferM' where M is the index of the tranfer matrix that value is associated with.
#' @param timeArr numeric array defining time points we are interested in looking at.
#' @param cModel function that accepts the par parameters and define the carbon model.
#' @param verbose boolean flag for extra error messages
#'
#' @return a data.frame with the time and values for the carbon pools and CO2
#' @export
#' @import deSolve assertthat
#'
runModel <- function(par, timeArr, cModel, verbose=FALSE){
  
  ##dummy check
  are_equal(class(par), 'numeric')
  are_equal(class(names(par)), 'character')
  are_equal(class(timeArr), 'numeric')
  are_equal(class(cModel), 'function')
  are_equal(class(verbose), 'boolean')
  
  timeArr <- sort(unique(c(0, timeArr, timeArr+1)))
  
  labelArr <- gsub('\\.?a1$', '', names(par)[grepl('a1$', names(par))])
  tau.arr <- grepl('tau\\d+$', names(par))
  transfer.arr <- grepl('transfer\\d+$', names(par))
  
  ans <- data.frame()
  for(labelStr in labelArr){
    
    aN <- grepl(sprintf('^%s.?a', labelStr), names(par))
    
    y0 <- C_bulk*par[aN]
    y0 <- c(y0, C_bulk-y0)
    names(y0) <- sprintf('C%d', 1:length(y0))
    
    model <- lsoda(y=y0, times=timeArr, func=cModel,
                   parms=list(tau=par[tau.arr], transfer=par[transfer.arr]))
    if(verbose) print(head(model))
    
    model$label <- labelStr
    
    ans <- rbind(ans, model)
  }
  return(ans)
}
