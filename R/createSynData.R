#' Create synthetic 'data'
#' 
#' This function creates model output with a specified parameter set and adds a relative uncertainty. This is a good example of the format your data is expected to be in for this package.
#'
#' @param par numeric named parameter set defining the run
#' @param timeArr numeric time points of interest
#' @param C_bulk numeric scalar of inital bulk carbon
#' @param dt numeric scalar 'cap' time for dCO2 calculation
#' @param tauStr character string defining the turnover parameters in par
#' @param transStr character string defining the transfer parameters in par
#' @param allocationStr character string defining the allocation parameters in par
#' @param relTime list of C pools to normalize and their associated normalization time points
#' @param relSd.ls list of relative standard deviation to assign to dCO2 and relative C pools
#' @param verbose print out useful debugging statetments
#'
#' @return a data.frame with synthetic data
#' @export
#'
#' @examples
#' synData <- createSynData(par=unlist(list('label1.a1'=0.1, tau1=180, tau2=10*365)), timeArr=2^(seq(0, 10, length=50)))
#' print(head(synData))
createSynData <- function(par, timeArr, C_bulk=1, dt=1, tauStr='tau', transStr='trans', allocationStr='a', relTime=list(), relSd.ls=list(dCO2=0.05, relC=0.1), verbose=FALSE){
  modelOutput <- runModel(par=par, timeArr=timeArr, 
                          C_bulk=C_bulk, dt=dt,
                          tauStr=tauStr, transStr=transStr, allocationStr=allocationStr, 
                          verbose=verbose)
  
  relNames <- sprintf('rel%s', names(relTime))
  for(nameStr in names(relTime)){
    modelOutput[,sprintf('rel%s', nameStr)] <- modelOutput[,nameStr]/modelOutput[which.min(abs(modelOutput$time - relTime[[nameStr]])), nameStr]
  }
  
  if(sum(grepl('^C\\d+$', names(modelOutput))) > 1){
    modelOutput$bulkC <- rowSums(modelOutput[,grepl('^C\\d+$', names(modelOutput))])
  }else{
    modelOutput$bulkC <- modelOutput$C1
  }
  
  modelOutput$CO2 <- C_bulk - modelOutput$bulkC
  temp <- modelOutput[,c('time', 'CO2')]
  temp$time <- temp$time-dt
  names(temp) <- c('time', 'nextCO2')
  temp <- merge(modelOutput[,c('time', 'CO2')], temp)
  temp$dCO2 <- (temp$nextCO2-temp$CO2)/dt
  modelOutput <- merge(modelOutput, temp[,c('time', 'dCO2')], all=TRUE)
  
  ##Construct artifical data
  refData <- as.data.frame(modelOutput[,c('time', 'label', relNames, 'dCO2')])
  #refData[refData$time != , 'relC1'] <- NA
  for(nameStr in relNames){
    refData[,sprintf('%s.mean', nameStr)] <- refData[,nameStr]*rnorm(dim(refData)[1], mean=1, sd=relSd.ls$relC)
    refData[,sprintf('%s.sd', nameStr)] <- refData[,nameStr]*relSd.ls$relC
  }
  refData$dCO2.mean <- refData$dCO2*rnorm(dim(refData)[1], mean=1, sd=relSd.ls$dCO2)
  refData$dCO2.sd <- refData$dCO2*relSd.ls$dCO2
  
  refData.mean <- melt(refData[,c('time', 'label', names(refData)[grepl('mean$', names(refData))])], id.vars = c('time', 'label'), value.name='mean')
  refData.mean$variable <- gsub('\\.mean$', '', refData.mean$variable)
  refData.mean <- refData.mean[is.finite(refData.mean$mean), ]
  refData.sd <- melt(refData[,c('time', 'label', names(refData)[grepl('sd$', names(refData))])], id.vars=c('time', 'label'), value.name='sd')
  refData.sd$variable <- gsub('\\.sd$', '', refData.sd$variable)
  refData <- merge(refData.mean, refData.sd)
 
  return(refData) 
}