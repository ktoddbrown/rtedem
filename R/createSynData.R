#' Create synthetic 'data'
#' 
#' This function creates model output with a specified parameter set and adds a relative uncertainty. This is a good example of the format your data is expected to be in for this package.
#'
#' @param par numeric named parameter set defining the run
#' @param reactionNetwork data.frame with the 'to', 'from', and 'reaction' columns
#' @param timeArr numeric time points of interest
#' @param allocationFn a function that returns the initial carbon distribution based on par
#' @param dt numeric scalar 'cap' time for dCO2 calculation
#' @param relTime named array of C pools to normalize and their associated normalization time points
#' @param relSd the relative standard deviation
#' @param verbose print out useful debugging statetments
#'
#' @return a data.frame with synthetic data
#' @export
#'
createSynData <- function(par, reactionNetwork, allocationFn, timeArr, dt, relTime=c(), relSd=0.1, verbose=FALSE){
  
  relTime <- unlist(relTime)
  
  modelOutput <- runModel(par=par, timeArr=timeArr, y0=allocationFn(par), dt=dt, reactionNetwork=reactionNetwork, verbose=verbose)
  
  if(verbose){print(head(modelOutput))}
  
  #calcuate any relative temporal changes
  relNames <- sprintf('rel%s', names(relTime))
  for(nameStr in names(relTime)){
    modelOutput[,sprintf('rel%s', nameStr)] <- modelOutput[,nameStr]/modelOutput[which.min(abs(modelOutput$time - relTime[nameStr])), nameStr]
  }
  
  #calculate dCO2
  temp <- modelOutput[,c('time', 'CO2')]
  temp$time <- temp$time-dt
  names(temp) <- c('time', 'nextCO2')
  temp <- merge(modelOutput[,c('time', 'CO2')], temp)
  temp$dCO2 <- (temp$nextCO2-temp$CO2)/dt
  modelOutput <- merge(modelOutput, temp[,c('time', 'dCO2')], all=TRUE)
  
  ##Construct artifical data
  refData <- modelOutput
  for(nameStr in setdiff(names(modelOutput), 'time')){
    refData[,sprintf('%s.mean', nameStr)] <- refData[,nameStr]*rnorm(dim(refData)[1], mean=1, sd=relSd)
    refData[,sprintf('%s.sd', nameStr)] <- refData[,nameStr]*relSd
  }
  
  refData.mean <- melt(refData[,c('time', names(refData)[grepl('mean$', names(refData))])], id.vars = c('time'), value.name='mean')
  refData.mean$variable <- gsub('\\.mean$', '', refData.mean$variable)
  refData.mean <- refData.mean[is.finite(refData.mean$mean), ]
  refData.sd <- melt(refData[,c('time', names(refData)[grepl('sd$', names(refData))])], id.vars=c('time'), value.name='sd')
  refData.sd$variable <- gsub('\\.sd$', '', refData.sd$variable)
  refData <- merge(refData.mean, refData.sd)
 
  return(refData) 
}