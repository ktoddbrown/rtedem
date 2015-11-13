#' Qunantify model data fits
#' 
#' Designed for optim or modMCMC from the FME package, this function quantifies model data fit by scoring the data by case (refData$label) and data type (refData$variable) grouped by time breaks (temporalSplit). If return.df is true it returns a measure data.frame defined by these three factors, otherwise these groups scores are divided by the number of data in each group to generate a mean score per group, averaged, and then multiplied by the total number of data points.
#'
#' @param par a named array for parameters.
#' @param refData a data frame with (time, label, variable, mean, sd) as column names, refData$variables can be c(C\\d+, relC\\d, CO2, dCO2).
#' @param reactionNetwork a data frame defining the dC model.
#' @param allocationFn a funciton returning the inital carbon distribution from the previous par
#' @param temporalSplit numeric with temporal splits for measure function
#' @param measureFn measure function used
#' @param dt cap time for measure comparison
#' @param verbose boolean for debug statements
#' @param return.df boolean to flag a full measure data frame return instead of a single summary values
#' @param weighByCount boolean to flag if we are weighing the mean measures by the number of data points.
#'
#' @return a single numeric measure of the model fit to reference data or the full data.frame if flagged
#' @export
#' @import plyr reshape2 assertthat

measureModelFit <- function(par, refData, reactionNetwork, allocationFn,
                         temporalSplit = c(), measureFn=ll.measure, 
                         dt=1,
                         weighByCount=TRUE, return.df=FALSE, verbose=FALSE){

   
  assert_that(is.logical(verbose) && is.logical(return.df))
  assert_that(is.function(measureFn))
  assert_that(is.data.frame(reactionNetwork))
  assert_that(is.numeric(par))
  assert_that(is.numeric(temporalSplit) || length(temporalSplit)==0)
  assert_that(is.data.frame(refData))
  
  assert_that(all(c('time', "variable", "mean", "sd") %in% names(refData) ))
  
  refData$variable <- as.character(refData$variable)
  
  ##run model
  modelOutput <- runModel(par=par, timeArr=unique(refData$time), y0=allocationFn(par), dt=dt, reactionNetwork=reactionNetwork, verbose=verbose)
    
  ##generate relative differences
  if(any(grepl('rel', unique(refData$variable) ))){
      relativePoints <- refData[grepl('rel', refData$variable),]
      normTimes <- relativePoints[relativePoints$mean == 1,]
      assert_that(all.equal(unique(relativePoints$variable), unique(normTimes$variable)))
      relOutput <- ldply(unique(relativePoints$variable), function(varStr){
        ans <- data.frame(modelOutput[, gsub('rel', '', varStr)]/modelOutput[modelOutput$time == normTimes[normTimes$variable %in% varStr, 'time'], gsub('rel', '', varStr)])
        names(ans) <- varStr
        ans[modelOutput$time == normTimes[normTimes$variable %in% varStr, 'time'], varStr] <- NA
        return(ans)
      })
      modelOutput <- cbind(modelOutput, relOutput)
  }
    
    #calculate dCO2
    temp <- modelOutput[,c('time', 'CO2')]
    temp$time <- temp$time-dt
    names(temp) <- c('time', 'nextCO2')
    temp <- merge(modelOutput[,c('time', 'CO2')], temp)
    temp$dCO2 <- (temp$nextCO2-temp$CO2)/dt
    modelOutput <- merge(modelOutput, temp[,c('time', 'dCO2')], all=TRUE)
    
    #create df form point comparison
    evalPoints <- merge(refData, 
                        melt(modelOutput[, c('time', unique(refData$variable))], id.vars=c('time'), value.name='model'))
    
    
    if(dim(evalPoints)[1] != dim(refData)[1]){
      if(verbose){cat('reference Data:\n'); print(refData)}
      if(verbose){cat('evaluation points:\n'); print(evalPoints)}
      stop('Can not match reference data with model outputs.')
    }
    
    #split by time points
    evalPoints$timeGroups <- cut(evalPoints$time, breaks=c(-Inf, temporalSplit, Inf))
    
    if(verbose){cat('evaluation points:\n'); print(evalPoints)}
    
    #find measure dataframe
    measure.df <- ddply(evalPoints, c('variable', 'timeGroups'), function(xx){
                        data.frame(measure=measureFn(model=xx$model, data=xx$mean, sd=xx$sd),
                                     count=sum(is.finite(xx$mean+xx$sd+xx$model)))})
    if(verbose){cat('measure.df:\n'); print(measure.df)}
    
    ans <- mean((measure.df$measure/measure.df$count))
    if(weighByCount){
      ans <- ans*sum(measure.df$count)
    }
    if(verbose){cat('measure: ', ans)}
    
    if(return.df){
      return(measure.df)
    }else{
      return(ans)
    }
}