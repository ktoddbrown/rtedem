#' Run a decomposition model
#'
#' @description Run a model across several different cases defined by different applocation parameters but which share the same decomposition and transfer parameters.
#' 
#' @param par a named numeric array. The allocation parameters end with 'aD' where D is an integer identifying the associated pool, and the pool name starts with 'lableN.' where lableN is the case lable. The turnover parameters are named 'turnoverD' where D is an integer identifying the associated pool. The transfer parameters are named 'transferM' where M is the index of the tranfer matrix that value is associated with.
#' @param timeArr numeric array defining time points we are interested in looking at.
#' @param reactionNetwork a data.frame defining the model
#' @param y0 a named array with the inital carbon distribution
#' @param dt numeric defining the cap time to calculate the dCO2 flux
#' @param verbose boolean flag for extra error messages
#'
#' @return a data.frame with the time and values for the carbon pools and CO2
#' @export
#' @import deSolve assertthat
#'
runModel <- function(par, timeArr, reactionNetwork, y0, dt=1, verbose=FALSE){
  
  ##dummy check
  are_equal(class(par), 'numeric')
  are_equal(class(names(par)), 'character')
  are_equal(class(timeArr), 'numeric')
  are_equal(class(verbose), 'boolean')
  
  timeArr <- sort(unique(c(0, timeArr, timeArr+dt)))
  
  if(verbose) {cat('y0:\n'); print(y0)}
  if(verbose) {cat('parms:\n'); print(par)}
  #if(verbose) {cat('decayK:\n'); print(makeDecompMatrix(par, ...))}
  model <- as.data.frame(lsoda(y=y0, times=timeArr, func=dCModel, parms=par, reactionNetwork=reactionNetwork, verbose=verbose))
  
  if(verbose) print(summary(model))
  #model <- data.frame(a=1)
  if(verbose) print(head(model))
  
  return(model)
}
