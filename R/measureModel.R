##optimization function for using CO2 and fast pool ratio data
CO2bio.all.opt.fn <- function(par, refData, ratioPool = 1, #1=fast, 2=slow, 3=passive
                          C_bulk=1, #total intial soil carbon
                          measureFn=ll.measure, 
                          measurePools=c('dCO2'), #CO2:cumulative C-CO2, dCO2:daily CO2 flux, 
                                                 #fast:fast carbon pool, dCO2.young:daily CO2 flux <=6 months, 
                                                 #dCO2.old:daily CO2 flux >6 months
                          youngThreadhold=10, #cut off for young carbon pool
                          measureType=c('mean'), #mean:normalize score to data type and case, simple:simple sum
                          ##TODO add an LL weight?
                          debug=FALSE){

   
    
    if(any((par[grepl('a1$', names(par))] + par[grepl('a2$', names(par))] ) > 1)){
                                        #Check that we don't allocate
                             #...100% or more to the first two pools
      if(debug) print(par)
      if(debug) print((par[grepl('a1$',names(par))] + par[grepl('a2$',names(par))] ))
      if(debug) cat('bad allocation\n')
        return(1e32)         #If we do then return an impossibly huge
                             #...number since INF breaks modMCMC
    }
    transfer <- par[grepl('transfer', names(par))]
    transferMatrix <- matrix(c(-1, transfer[1], transfer[2], 
                               transfer[3], -1, transfer[4], 
                               transfer[5], 0, -1), nrow=3)
    if(any(colSums(transferMatrix) > 0)){
      if(debug) print(par)
      if(debug) print(transferMatrix)
      if(debug) cat('bad transfers, violate conservation of mass\n')
      return(1e32)
    }

    if(all(is.na(refData$ratio.mean))){
      biomassRefTime <- 14
    }else{
      biomassRefTime <- min(refData$time[is.finite(refData$ratio.mean)])
    }
    modelOutput <- CO2bio.fn(par=par, myTime=unique(refData$time), 
                             biomassRefTime=biomassRefTime,
                             ratioPool=ratioPool, C_bulk=C_bulk)
    #modelOutput <- CO2bio.fn(par=par.true, myTime=unique(refData$time), biomassRefTime=14)
    
    evalPoints <- merge(refData, modelOutput, all=TRUE)
    #if(debug) print(evalPoints)
    measure.df <- ddply(evalPoints, c('caseID'), function(xx){
      if(measureType %in% 'simple'){
        norm <- list('dCO2'=1, 'CO2'=1, 'fast'=1, 
                  'dCO2.young'=1, 'CO2.young'=1, 'fast.young'=1,
                  'dCO2.old'=1, 'CO2.old'=1, 'fast.old'=1)
      }else if(measureType %in% 'mean'){
        norm <- list('dCO2'=sum(is.finite(xx$dC.mean)), 
                     'CO2'=sum(is.finite(xx$C.mean)), 
                     'fast'=sum(is.finite(xx$ratio.sd)), 
                  'dCO2.young'=sum(is.finite(xx$dC.mean[xx$time <= youngThreadhold])), 
                  'CO2.young'=sum(is.finite(xx$C.mean[xx$time <= youngThreadhold])), 
                  'fast.young'=sum(is.finite(xx$ratio.mean[xx$time <= youngThreadhold])),
                  'dCO2.old'=sum(is.finite(xx$dC.mean[xx$time > youngThreadhold])), 
                  'CO2.old'=sum(is.finite(xx$C.mean[xx$time > youngThreadhold])), 
                  'fast.old'=sum(is.finite(xx$ratio.mean[xx$time > youngThreadhold])))
      }
      
      #norm[[!is.finite(norm)]] <- NA
#       if(debug) print(measureFn(model=xx$biomass, data=xx$ratio.mean, sd=xx$ratio.sd)/norm$fast)
#       if(debug) print( measureFn(model=xx$biomass[xx$time <= youngThreadhold], 
#                                  data=xx$ratio.mean[xx$time <= youngThreadhold], 
#                                  sd=xx$ratio.sd[xx$time <= youngThreadhold])/norm$fast.young)
#       if(debug) print(measureFn(model=xx$biomass[xx$time > youngThreadhold], 
#                                 data=xx$ratio.mean[xx$time > youngThreadhold], 
#                                 sd=xx$ratio.sd[xx$time > youngThreadhold])/norm$fast.old)
      ans <- data.frame(
      dCO2 = measureFn(model=xx$dCO2, data=xx$dC.mean, sd=xx$dC.sd)/norm$dCO2,
      CO2 = measureFn(model=xx$CO2, data=xx$C.mean, sd=xx$C.sd)/norm$CO2,
      fast = measureFn(model=xx$biomass, data=xx$ratio.mean, sd=xx$ratio.sd)/norm$fast,
      dCO2.young = measureFn(model=xx$dCO2[xx$time <= youngThreadhold], 
                                  data=xx$dC.mean[xx$time <= youngThreadhold], 
                                  sd=xx$dC.sd[xx$time <= youngThreadhold])/norm$dCO2.young,
      CO2.young = measureFn(model=xx$CO2[xx$time <= youngThreadhold], 
                                 data=xx$C.mean[xx$time <= youngThreadhold], 
                                 sd=xx$C.sd[xx$time <= youngThreadhold])/norm$CO2.young,
      fast.young = measureFn(model=xx$biomass[xx$time <= youngThreadhold], 
                                  data=xx$ratio.mean[xx$time <= youngThreadhold], 
                                  sd=xx$ratio.sd[xx$time <= youngThreadhold])/norm$fast.young,
      dCO2.old = measureFn(model=xx$dCO2[xx$time > youngThreadhold], 
                                data=xx$dC.mean[xx$time > youngThreadhold], 
                                sd=xx$dC.sd[xx$time > youngThreadhold])/norm$dCO2.old,
      CO2.old = measureFn(model=xx$CO2[xx$time > youngThreadhold], 
                               data=xx$C.mean[xx$time > youngThreadhold], 
                               sd=xx$C.sd[xx$time > youngThreadhold])/norm$CO2.old,
      fast.old = measureFn(model=xx$biomass[xx$time > youngThreadhold], 
                                data=xx$ratio.mean[xx$time > youngThreadhold], 
                                sd=xx$ratio.sd[xx$time > youngThreadhold])/norm$fast.old)
      ans <- ans/length(measurePools)*length(refData$time)
      return(ans)
    })

    if(debug) {cat('measure df:\n'); print(measure.df)}
    
    if(debug) {cat('measures:\n'); print(measure.df[,measurePools])}
    if(debug) cat('measure: ', 
                  sum(unlist(measure.df[,measurePools]), na.rm=TRUE)
                  ,'\n')
    return(sum(unlist(measure.df[,measurePools]), na.rm=TRUE))
}