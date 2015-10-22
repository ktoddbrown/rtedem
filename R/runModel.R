#' Run a decomposition model
#'
#' @description Run a model across several different cases defined by different applocation parameters but which share the same decomposition and transfer parameters.
#' 
#' @param par a named numeric array. The allocation parameters end with 'aD' where D is an integer identifying the associated pool, and the pool name starts with 'lableN.' where lableN is the case lable. The turnover parameters are named 'turnoverD' where D is an integer identifying the associated pool. The transfer parameters are named 'transferM' where M is the index of the tranfer matrix that value is associated with.
#' @param timeArr numeric array defining time points we are interested in looking at.
#' @param cModel function that accepts the par parameters and define the carbon model.
#' @param C_bulk numeric total inital bulk carbon
#' @param dt numeric defining the cap time to calculate the dCO2 flux
#' @param tauStr string defining the regexp flagging the turnover times in par
#' @param transStr string defining the regexp flagging the transfer propotions in par
#' @param allocationStr string defining the allocation proportions in par
#' @param verbose boolean flag for extra error messages
#'
#' @return a data.frame with the time and values for the carbon pools and CO2
#' @export
#' @import deSolve assertthat
#'
runModel <- function(par, timeArr, cModel=dC.firstOrderModel, C_bulk=1, dt=1,
                     tauStr='tau', transStr='transfer', allocationStr='a', verbose=FALSE){
  
  ##dummy check
  are_equal(class(par), 'numeric')
  are_equal(class(names(par)), 'character')
  are_equal(class(timeArr), 'numeric')
  are_equal(class(cModel), 'function')
  are_equal(class(verbose), 'boolean')
  
  timeArr <- sort(unique(c(0, timeArr, timeArr+dt)))
  
  labelArr <- gsub(sprintf('\\.?%s1$', allocationStr), '', 
                   names(par)[grepl(sprintf('\\.?%s1$', allocationStr), names(par))])
  if(length(labelArr)==0){
    labelArr <- c('default')
  }
  ans <- data.frame()
  for(labelStr in labelArr){
    
    aN <- grepl(sprintf('^%s.?%s', labelStr, allocationStr), names(par))
    if(verbose){cat('aN:\n'); print(aN)}
    
    y0 <- C_bulk*par[aN]
    y0 <- c(y0, C_bulk-sum(y0))
    names(y0) <- sprintf('C%d', 1:length(y0))
    if(verbose) {cat('y0:\n'); print(y0)}
    if(verbose) {cat('parms:\n'); print(par)}
    if(verbose) {cat('decayK:\n'); print(makeDecompMatrix(par, tauStr=tauStr, transStr=transStr))}
    model <- as.data.frame(lsoda(y=y0, times=timeArr, func=cModel, parms=par, tauStr=tauStr, transStr=transStr))
    if(verbose) print(summary(model))
    #model <- data.frame(a=1)
    if(verbose) print(head(model))
    
    model$label <- labelStr
    
    ans <- rbind(ans, model)
  }
  return(ans)
}
