dC.guildSCEBModel <- function(t, y, parms, 
                           poolDef=list(B1=list(cSource=c('S1', 'S2'), enz='E1', dead='C2'),
                                        B2=list(cSource=c('S2'), enz='E2', dead='C2'),
                                        B3=list(cSource=c('S1', 'S2'), dead='C2')),
                           enzKinetics=list(E1='MM', E2='multi'),
                           uptakeKinetics='monod'){
  
  poolNames <- unique(c(names(poolDef), unlist(poolDef), gsub('E', 'C', names(enzKinetics))))
  poolNames <- c(poolNames, sprintf('P%s', poolNames[grepl('(S|E|C)', poolNames)]))
  assert_that(length(y) == length(poolNames))
  
  names(y) <- poolNames
  
  ##death/turnover
  turnover <- parms[grepl('turnover', names(parms))]
  names(turnover) <- gsub('\\.', '', gsub('turnover', '', names(turnover)))
  turnover <- turnover * y[names(turnover)]
 
  ##uptake
  uptake.df <- data.frame(parms[grepl('(v|km)_up', names(parms))])
  names(uptake.df) <- c('value')
  uptake.df$key <- row.names(uptake.df)
  uptake.df$to <- gsub('\\..+\\..+$','', uptake.df$key)
  uptake.df$parName <- gsub('^.+\\.', '' ,gsub('\\.[^\\.]+$','', uptake.df$key))
  uptake.df$from <- gsub('^[^\\.]+\\.[^\\.]+\\.','', uptake.df$key)
  uptake.df <- dcast(uptake.df, to + from~parName)
  
  feeders <- lapply(unique(uptake.df$from), function(xx){return(unique(uptake.df$to[uptake.df$from == xx]))})
  names(feeders) <- unique(uptake.df$from) 
  if(grepl('monod', uptakeKinetics)){
    #sum the 
    uptake.df$rate <- y[uptake.df$to]*y[uptake.df$from]*uptake.df$v_up/(y[uptake.df$from])
  }
  ##enzyme kinetics
  ##sorption/desoprtion
}

makeTestPar.guildDecomp <- function(poolDef=list(B1=list(cSource=c('S1', 'S2'), enz='E1', dead='C2'),
                                                 B2=list(cSource=c('S2'), enz='E2', dead='C2'),
                                                 B3=list(cSource=c('S1', 'S2'), dead='C2')),
                                    enzKinetics=list(E1='MM', E2='multi'),
                                    uptakeKinetics='Monod'){
  poolNames <- unique(c(names(poolDef), unlist(poolDef), gsub('E', 'C', names(enzKinetics))))
  poolNames <- c(poolNames, sprintf('P%s', poolNames[grepl('(S|E|C)', poolNames)]))
  
  biomassArg <- lapply(poolDef, function(xx){
    ans <- list()
    #Each biomass has its own turnover
    ans$turnover <- rnorm(1, mean=0.1, sd=0.05)
    
    #Each biomass/simple has its own uptake kinetic
    ans$v_up <- lapply(xx$cSource, function(yy){
      return(rnorm(1, mean=2, sd=0.5))
    })
    names(ans$v_up) <- xx$cSource
    
    ans$km_up <- lapply(xx$cSource, function(yy){
      return(rnorm(1, mean=10, sd=0.5))
    })
    names(ans$km_up) <- xx$cSource
    
    
    return(ans)
  })
  
  enzArg <- lapply(enzKinetics, function(xx){
    ans <- list()
  #Each enzyme (or complex) has its own enzyme kinitic
    ans$v_max <- rnorm(1, mean=1, sd=0.2)
    if(grepl('MM', xx)){
      ans$km <- rnorm(1, mean=15, sd=1)
    }
  #Each enzyme has its own turnover
    ans$turnover <- rnorm(1, mean=0.1, sd=0.01)
    return(ans)
  })
  
  #Each simple/enzyme/complex has it's own sorbtion/desorbtion
  protectionArg <- lapply(poolNames[grepl('^P', poolNames)], function(xx){
    return(list(rnorm(1, mean=0.5, sd=0.01)))
  })
  names(protectionArg) <- poolNames[grepl('^P', poolNames)]
  protectionArg$maxP <- 10
  
  return(unlist(c(biomassArg, enzArg, protectionArg)))
}