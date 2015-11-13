## ------------------------------------------------------------------------
library(ggplot2)
library(reshape2)
library(plyr)
library(Matrix)
library(rtedem)

## ----plotTurnover, fig.width=6-------------------------------------------
codedParameters <- publishedParameters()
minMaxBounds <- makeModelBounds(codedParameters)
turnovers <- ldply(names(minMaxBounds), function(xx){
  ans <- minMaxBounds[[xx]][grepl('K', minMaxBounds[[xx]]$name),]
  ans[,c('true', 'min', 'max')] <- ans[,c('true', 'min', 'max')] * -1
  ans$model <- xx
  return(ans)
})
turnovers$modelIndex <- as.numeric(as.factor(turnovers$model))
yAxis <- unique(turnovers[,c('model', 'modelIndex')])
ggplot(turnovers) + geom_rect(aes(ymax=modelIndex+0.45, ymin=modelIndex-0.45, xmax=min/365, xmin=max/365, fill=name)) + 
  geom_point(aes(y=modelIndex, x=true/365)) + 
  scale_x_log10() + scale_y_continuous(breaks=yAxis$modelIndex, labels=yAxis$model) +
  labs(title='Comparison of turnover times', y='', x='time [yr]') + guides(fill=FALSE)

## ------------------------------------------------------------------------
decayMatrix <- llply(codedParameters, function(xx){
  Turnover <- Matrix(diag(xx$tau/365))
  A <- matrix(data=0, nrow=dim(Turnover)[1], ncol=dim(Turnover)[2])
  A[as.numeric(gsub('A', '', xx$trans$name))] <- xx$trans$val
  return(list(turnover = Turnover, transfer=Matrix(A), K=Matrix(diag(1/(xx$tau/365)))))
})

## ------------------------------------------------------------------------
l_ply(names(codedParameters), function(xx){
  #cat(xx,' turnover [year]:\n');print(decayMatrix[[xx]]$turnover)
  cat(xx, ' transfer Matrix:\n');print(decayMatrix[[xx]]$transfer)
})

## ------------------------------------------------------------------------
l_ply(names(codedParameters), function(xx){
  cat(xx, 'decay Matrix (1/tau*transfer):\n')
  A <- decayMatrix[[xx]]$transfer
  diag(A) <- -1
  print(decayMatrix[[xx]]$K %*% A)
})

## ----helperFun-----------------------------------------------------------
makePlots <- function(par, pools, timeArr=10^seq(-1, 0, length=20)*365){
  totalC <- sum(unlist(pools))
  #poolIndex <- list(simple=1, dormant=2, protect_S=3, protect_C=4, biomass=5, enzyme=6, complex=7)
  poolIndex <- lapply(names(pools), function(xx){which(names(pools) %in% xx)})
  names(poolIndex) <- names(pools)
  
  parFull <- (unlist(pools)/totalC)[-max(unlist(poolIndex))]
  names(parFull) <- sprintf('a%d', 1:(length(parFull)))
  parFull <- c(unlist(par), parFull)
  
  poolOuts <- runModel(par=parFull, timeArr=timeArr, cModel=dC.biomassModel, C_bulk=totalC, dt=1, poolAssignment=poolIndex, rateFlags=list(enz='MM', uptake='MM', prod='uptake', sorb='carryCapacity'), allocationStr='a')
  poolOuts <- melt(poolOuts, id.vars=c('time', 'label'))
  ans <- list(pools = ggplot(poolOuts) + geom_line(aes(x=time, y=value))  + labs(title='Carbon pools', x='time [day]', y='C stock')+ facet_wrap(~variable, scales='free'))
  
  CO2Outs <- createSynData(par=parFull, timeArr=timeArr, cModel=dC.biomassModel, C_bulk=totalC, dt=1, poolAssignment=poolIndex, rateFlags=list(enz='MM', uptake='MM', prod='uptake', sorb='carryCapacity'), allocationStr='a')
  
  ans$CO2 <- ggplot(CO2Outs) + geom_line(aes(x=time, y=mean)) + geom_errorbar(aes(x=time, ymin=mean-sd, ymax=mean+sd)) +  labs(title='Synthetic respiration', x='time [day]', y='CO2 flux [g CO2-C g^-1 soil-C day^-1]')
  
  return(ans)
}


## ----setUpNonlinearModel-------------------------------------------------
par <- list(v_enz=5, km_enz=2,
                v_up=1, km_up=1, cue=0.75,
                basal=0.1,
                turnover_b=0.05,
                enzProd=0.01, enzCost=1, 
                turnover_e=0.005,
                dormancy_rate=1e-3, S_dormancy=0.1,
                sorb_rate=0.01, desorb_rate=0.01, carbonCapacity=50)
pools <- list(protect_S=0.3, protect_C=3)
##Assume dP_C and dP_S are equal to 0
pools$complex <- par$desorb_rate/par$sorb_rate*(1/pools$protect_C-1/par$carbonCapacity)^-1
pools$simple <- par$desorb_rate/par$sorb_rate*(1/pools$protect_S-1/par$carbonCapacity)^-1

##Assume dB equal to 0
pools$biomass <- pools$simple*par$v_up*(par$cue) - 
                      par$km_up*(par$basal+par$turnover_b)

##Assume most of microbes are dormant
##TODO dormancy seems to destablize model, not sure what's up with this
pools$dormant <- pools$biomass*10

##Assume that enzymes should be proportional to biomass
pools$enzyme <- pools$biomass*par$enzProd

## ------------------------------------------------------------------------
temp <- makePlots(par, pools[c('simple', 'complex', 'enzyme')])
temp$pools

## ------------------------------------------------------------------------
temp <- makePlots(par, pools[c('simple', 'complex', 'biomass')])
temp$pools; temp$CO2

## ------------------------------------------------------------------------
temp <- makePlots(par, pools[c('simple', 'complex', 'biomass', 'enzyme')])
temp$pools; temp$CO2

## ------------------------------------------------------------------------
temp <- makePlots(par, pools[c('simple', 'complex', 'protect_C', 'protect_S')])
temp$pools

## ------------------------------------------------------------------------
temp <- makePlots(par, pools[c('simple', 'complex', 'biomass', 'enzyme', 'protect_C', 'protect_S')])
temp$pools; temp$CO2

## ------------------------------------------------------------------------
temp <- makePlots(par, pools)
temp$pools; temp$CO2

