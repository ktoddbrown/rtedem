#' Change in carbon pools for a biomass decay model
#'
#' This models can have up to 7 pools in it including simple, complex, enzyme, biomass, dormant biomass, protected simple, and protected complex pools. Required are simple, complex and biomass.
#'
#' @param t numeric time (ignored)
#' @param y numeric carbon pools
#' @param parms list of named parameters
#' @param poolAssignment list of named pool indecies, only simple, complex and biomass pools are required to be defined.
#' @param rateFlags list of strings selecting rate options. Options are listed in defaults.
#' @param verbose logical flag to give useful debug statements
#' 
#' @return a list with the change in carbon pools declared in poolAssigments
#' @export
#' @import assertthat
dC.biomassModel <- function(t=0, y, parms, 
                         poolAssignment = list(simple=1, complex=2, enzyme=3, biomass=4, dormant=5, protect_S=6, protect_C=7),
                         rateFlags=list(enz=c('MM', 'revMM', 'multi')[1], 
                                        uptake=c('Monod', 'MM', 'revMM')[1], 
                                        prod=c('uptake', 'biomass', 'const')[1],
                                        sorb=c('carryCapacity')[1]),
                         verbose=FALSE){

  
  assert_that(all(names(poolAssignment) %in% c('simple', 'complex', 'enzyme', 'biomass', 'dormant', 'protect_S', 'protect_C')))
  assert_that(ifelse('dormant' %in% names(poolAssignment), ('biomass' %in% names(poolAssignment)), TRUE))
  
  assert_that(ifelse(all(c('protect_S', 'protect_C') %in% names(poolAssignment)), ('sorb' %in% names(rateFlags)), TRUE))
  assert_that(ifelse('biomass' %in% names(poolAssignment), ifelse('simple' %in% names(poolAssignment), 'uptake' %in% names(rateFlags), TRUE) & ifelse('simple' %in% names(poolAssignment) & 'complex' %in% names(poolAssignment), 'enz' %in% names(rateFlags), TRUE), TRUE))
  #assert_that(ifelse(all(c('complex', 'simple') %in% names(poolAssignment)), ifelse('enzyme' %in% names(poolAssignment), 'enz' %in% names(rateFlags), TRUE))
  
  assert_that(length(y) >= max(unlist(poolAssignment)))
  if(verbose) {print(names(parms))}
  if(!all(c(c('v_enz')['enz' %in% names(rateFlags)], 
                  c('km_enz')['enz' %in% names(rateFlags) && grepl('MM', rateFlags$enz)],
                  c('v_up', 'km_up','cue')[all(c('biomass', 'simple') %in% names(poolAssignment))],
                  c('basal')['biomass' %in% names(poolAssignment)],
                  c('turnover_b')[all(c('biomass', 'complex') %in% names(poolAssignment))],
                  c('enzProd', 'enzCost')[all(c('enzyme', 'biomass') %in% names(poolAssignment))], 
                  c('turnover_e')[all(c('enzyme', 'complex') %in% names(poolAssignment))],
                  c('dormancy_rate')['dormant' %in% names(poolAssignment)],
                  c('sorb_rate', 'desorb_rate', 'carbonCapacity')[all(c('protect_S', 'protect_C') %in% names(poolAssignment))]) %in% names(parms))){
    stop(paste('Can not find all expected parameters. Expected [', paste( c(c('v_enz')['enz' %in% names(rateFlags)],  c('km_enz')['enz' %in% names(rateFlags) && grepl('MM', rateFlags$enz)], c('v_up', 'km_up','cue')[all(c('biomass', 'simple') %in% names(poolAssignment))], c('basal')['biomass' %in% names(poolAssignment)], c('turnover_b')[all(c('biomass', 'complex') %in% names(poolAssignment))], c('enzProd', 'enzCost')[all(c('enzyme', 'biomass') %in% names(poolAssignment))], c('turnover_e')[all(c('enzyme', 'complex') %in% names(poolAssignment))], c('dormancy_rate')['dormant' %in% names(poolAssignment)], c('sorb_rate', 'desorb_rate', 'carbonCapacity')[all(c('protect_S', 'protect_C') %in% names(poolAssignment))]), collapse=', '), ']. Found [', paste(names(parms), collapse=', '), ']' ))
  }

  #assert_that(all(c('simple', 'complex', 'biomass') %in% names(poolAssignment)))
  
  dy <- rep(NA, length(y))
  if('biomass' %in% names(poolAssignment)) {B <- y[[poolAssignment$biomass]]; dB <- 0}
  if('enzyme' %in% names(poolAssignment)) {E <- y[[poolAssignment$enzyme]]; dE <- 0}
  if('complex' %in% names(poolAssignment)) {C <- y[[poolAssignment$complex]]; dC <- 0}
  if('simple' %in% names(poolAssignment)) {S <- y[[poolAssignment$simple]]; dS <- 0}
  if('dormant' %in% names(poolAssignment)) {D <- y[[poolAssignment$dormant]]; dD <- 0}
  if('protect_S' %in% names(poolAssignment)) {P_S <- y[[poolAssignment$protect_S]]; dP_S <- 0}
  if('protect_C' %in% names(poolAssignment)) {P_C <- y[[poolAssignment$protect_C]]; dP_C <- 0}

  ##Deal with biomass (uptake/growth, enzyme production, dormancy, necromass, maintance respiration)
  if('biomass' %in% names(poolAssignment)){
    ##uptake/growth
    if( 'simple' %in% names(poolAssignment)){
      if(verbose) cat('Uptake kinetics: ')
      if(grepl('^Monod$', rateFlags$uptake) | grepl('^MM$', rateFlags$uptake) ){
        if(verbose) cat('Monod ')
        uptake <- parms['v_up']*S*B/(parms['km_up']+S)
      }else if(grepl('^revMM$', rateFlags$uptake)){
        if(verbose) cat('reverse Monod ')
        uptake <- parms['v_up']*S*B/(parms['km_up']+B)
      }else{
        stop(sprintf('Bad uptake flag: %s',rateFlags$uptake) )
      }
      if(verbose) cat(uptake, '\n')
      dS <- -uptake
      dB <- parms['cue']*uptake
    }else{
      uptake <- 0
    }
    
    ##enzyme production
    if('enzyme' %in% names(poolAssignment)){
      if(verbose) cat('Enzyme production: ')
      if('prod' %in% names(rateFlags)){
        if(grepl('^uptake$', rateFlags$prod)){
          if(verbose) cat('proptional to uptake ')
          enzProd <- parms['enzProd']*uptake
        }else if(grepl('^biomass$', rateFlags$prod)){
          if(verbose) cat('proptional to biomass ')
          enzProd <- parms['enzProd']*B
        }else if(grepl('^const$', rateFlags$prod)){
          if(verbose) cat('constant ')
          enzProd <- parms['enzProd']
        }else{
          stop('Bad enzyme production flag.')
        }
        if(verbose) cat(enzProd, '\n')
        dE <- enzProd
        dB <- dB - (1+parms['enzCost'])*enzProd
      }
    }
    
    ##dormancy
    if('dormant' %in% names(poolAssignment)){
      if(verbose) cat('Dormancy ')
      if(dB <= 0 & dB < B){
        if(verbose) cat('sleep microbes ')
        dD <- parms['dormancy_rate']*B
        if(dD-dB > B){
          if(verbose) cat('to cap ')
          dD <- B+dB
        }
      }
      
      if(dB > 0){
        if(verbose) cat('wake microbes ')
        dD <- -1*parms['dormancy_rate']*D
      }
      if(verbose) cat('dD: ', dD, '\n')
      dB <- dB - dD
    }
    
    ##necromass
    if('complex' %in% names(poolAssignment)){
      if(verbose) cat('Death: ', parms['turnover_b']*B, '\n')
      dB <- dB - parms['turnover_b']*B 
      dC <- dC + parms['turnover_b']*B 
    }
    
    ##maintaince respiration
    if(verbose) cat('Maintance resp: ', parms['basal']*B, '\n')
    dB <- dB - parms['basal']*B
  }
  
  ##Deal with enzyme turnover
  if(all(c('enzyme', 'complex') %in% names(poolAssignment))){
    if(verbose) cat('Enzyme turnover: ', parms['turnover_e']*E, '\n')
    dE <- dE - parms['turnover_e']*E
    dC <- dC + parms['turnover_e']*E
  }
  
  ##Deal with conversion of complex to simple, requiring either an enzyme or biomass pool
  if(all(c('simple', 'complex') %in% names(poolAssignment)) & any(c('biomass', 'enzyme') %in% names(poolAssignment))){
    if(verbose)cat('converting complex to simple via ')
    if('enzyme' %in% names(poolAssignment)){
      if(verbose) cat('enzyme ')
      Eproxy <- E
    }else if('biomass' %in% names(poolAssignment)){
      if(verbose) cat('biomass ')
      Eproxy <- B
    }
    
    if(grepl('^MM$', rateFlags$enz)){
      if(verbose) cat('M-M kinetics: ')
      enzCat <- parms['v_enz']*C*Eproxy/(parms['km_enz']+C)
    }else if(grepl('^revMM$', rateFlags$enz)){
      if(verbose) cat('revM-M kinetics: ')
      enzCat <- parms['v_enz']*C*Eproxy/(parms['km_enz']+Eproxy)
    }else if(grepl('^multi$', rateFlags$enz)){
      if(verbose) cat('multiplictive kinetics: ')
      enzCat <- parms['v_enz']*C*Eproxy
    }else{
      stop('Bad enzyme flag.')
    }
    if(verbose) cat(enzCat, '\n')
    dC <- dC - enzCat
    dS <- dS + enzCat
  }
  
  if('protect_S' %in% names(poolAssignment)){
    P_tot <- P_S
  }else{
    P_tot <- 0
  }
  if('protect_C' %in% names(poolAssignment)){
    P_tot <- P_tot + P_C
  }
  
  ##Deal with protection of S and C
  if(all(c('protect_S', 'simple') %in% names(poolAssignment))){
    if(verbose) cat('(De)sorbtion kinetics:')
    if(grepl('^carryCapacity$', rateFlags$sorb)){
    dP_S <- S*parms['sorb_rate']*(1-(P_tot)/parms['carbonCapacity']) - 
      P_S*parms['desorb_rate']
    dS <- dS - dP_S
    }else{
      stop('Bad rateFlags$sorb: ', rateFlags$sorb)
    }
  }
  if(all(c('protect_C', 'complex') %in% names(poolAssignment))){
    if(verbose) cat('(De)sorbtion kinetics:')
    if(grepl('^carryCapacity$', rateFlags$sorb)){
      dP_C <- C*parms['sorb_rate']*(1-(P_tot)/parms['carbonCapacity']) - 
        P_C*parms['desorb_rate']
      dC <- dC - dP_C
    }else{
      stop('Bad rateFlags$sorb: ', rateFlags$sorb)
    }
  }
  
  if('biomass' %in% names(poolAssignment)) {dy[[poolAssignment$biomass]] <- dB}
  if('enzyme' %in% names(poolAssignment)) {dy[[poolAssignment$enzyme]] <- dE}
  if('complex' %in% names(poolAssignment)) {dy[[poolAssignment$complex]] <- dC}
  if('simple' %in% names(poolAssignment)) {dy[[poolAssignment$simple]] <- dS}
  if('dormant' %in% names(poolAssignment)) {dy[[poolAssignment$dormant]] <- dD}
  if('protect_S' %in% names(poolAssignment)) {dy[[poolAssignment$protect_S]] <- dP_S}
  if('protect_C' %in% names(poolAssignment)) {dy[[poolAssignment$protect_C]] <- dP_C}
  
  names(dy)[unlist(poolAssignment)] <- names(poolAssignment)
  
  return(list(dy))
}