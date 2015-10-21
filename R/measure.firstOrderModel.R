#' Qunantify model data fits
#' 
#' Designed for optim or modMCMC from the FME package, this function quantifies model data fit by scoring the data by case (refData$label) and data type (refData$variable) grouped by time breaks (temporalSplit). If return.df is true it returns a measure data.frame defined by these three factors, otherwise these groups scores are divided by the number of data in each group to generate a mean score per group, averaged, and then multiplied by the total number of data points.
#'
#' @param par a named array for allocation, turnover times, and transfer rates for a first order linear decay model.
#' @param refData a data frame with (time, label, variable, mean, sd) as column names, refData$variables can be c(C\\d+, relC\\d, CO2, dCO2).
#' @param relTime named array for normalization time for temporally normalized measure pools
#' @param temporalSplit numeric with temporal splits for measure function
#' @param C_bulk inital soil carbon stock
#' @param measureFn measure function used
#' @param dt cap time for measure comparison
#' @param tauStr string defining the turnover time names in par
#' @param transStr string defining the transfer coeffcient names in par
#' @param allocationStr string defining the allocation names in par
#' @param verbose boolean for debug statements
#' @param return.df boolean to flag a full measure data frame return instead of a single summary values
#'
#' @return a single numeric measure of the model fit to reference data
#' @export
#' @import plyr reshape2 assertthat

measure.firstOrderModel <- function(par, refData, relTime = list(),
                         temporalSplit = c(), measureFn=ll.measure, 
                         dt=1, C_bulk=1, tauStr='tau', transStr='transfer', allocationStr='a',
                         verbose=FALSE, return.df=FALSE){

   
  assert_that(is.character(tauStr) && is.character(transStr) && is.character(allocationStr))
  assert_that(is.logical(verbose) && is.logical(return.df))
  assert_that(is.function(measureFn))
  assert_that(is.list(relTime))
  assert_that(is.numeric(par) && is.numeric(temporalSplit))
  assert_that(is.data.frame(refData))
  
  assert_that(all(c('time', "label", "variable", "mean", "sd") %in% names(refData) ))
  assert_that(all(grepl('(C\\d+)|(relC\\d)|(^CO2$)|(^dCO2$)', unique(refData$variable))))
  
  
    modelOutput <- runModel(par=par, myTime=unique(refData$time), C_bulk=C_bulk, dt=dt, tauStr=tauStr, transStr=transStr, allocationStr=allocationStr, verbose=verbose)
    
    modelOutput[,sprintf('rel%s', names(relTime))] <- t(ldply(names(relTime),function(nameStr){ modelOutput[,nameStr]/modelOutput[modelOutput$time == relTime[[nameStr]], nameStr]}))
    
    modelOutput$CO2 <- C_bulk - rowSums(modelOutput[,grepl('^C\\d+$', names(modelOutput))])
    temp <- modelOutput[,c('time', 'CO2')]
    temp$time <- temp$time-dt
    names(temp) <- c('time', 'nextCO2')
    temp <- merge(modelOutput[,c('time', 'CO2')], temp)
    temp$dCO2 <- (temp$nextCO2-temp$CO2)/dt
    modelOutput <- merge(modelOutput, temp[,c('time', 'dCO2')], all=TRUE)
    
    evalPoints <- merge(refData, 
                        melt(modelOutput[, c('time', 'label', unique(refData$variable))], id.vars=c('time', 'label'), value.name='model'))
    
    evalPoints$timeGroups <- cut(evalPoints$time, breaks=c(-Inf, temporalSplit, Inf))
    
    measure.df <- ddply(evalPoints, c('label', 'variable', 'timeGroups'), summarize,
                        measure=measureFn(model=model, data=mean, sd=sd),
                        count=sum(is.finite(mean+sd+model)))
    if(verbose){cat('measure.df:\n'); print(measure.df)}
    
    ans <- mean((measure.df$measure/measure.df$count))*sum(measure.df$count)
    if(verbose){cat('measure: ', ans)}
    
    if(return.df){
      return(measure.df)
    }else{
      return(ans)
    }
}