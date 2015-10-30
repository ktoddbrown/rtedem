#' Change in carbon pools for a Simple-Complex-Enzyme-Active-Dormant (biomass) decay model
#'
#' @param t numeric time (ignored)
#' @param y numeric carbon pools
#' @param parms list of named parameters
#' @param poolAssignment list of named pool indecies, only simple, complex and biomass pools are required to be defined.
#' @param rateFlags list of strings selecting rate options. Options are listed in defaults.
#' @param verbose logical flag to give useful debug statements
#' 
#' @return a list with the change in carbon in SCEB models
#' @export
#' @import assertthat
dC.biomassModel <- function(t, y, parms, 
                         poolAssignment = list(simple=1, complex=2, enzyme=3, biomass=4, dormant=5),
                         rateFlags=list(enz=c('MM', 'revMM', 'multi')[1], 
                                        uptake=c('Monod', 'MM', 'revMM')[1], 
                                        prod=c('uptake', 'biomass', 'const')[1]),
                         verbose=FALSE){

  
  assert_that(all(names(poolAssignment) %in% c('simple', 'complex', 'enzyme', 'biomass', 'dormant')))
  assert_that(length(y) >= max(unlist(poolAssignment)))
  if(verbose) {print(names(parms))}
  assert_that(all(c('v_enz', c('km_enz')[grepl('MM', rateFlags$enz)],
                    'v_up', 'km_up','cue', 'basal', 'turnover_b', 
                    c('enzProd', 'enzCost', 'turnover_e')['enzyme' %in% names(poolAssignment)],
                    c('dormancy_rate')['dormant' %in% names(poolAssignment)]) %in% names(parms)))

  assert_that(all(c('simple', 'complex', 'biomass') %in% names(poolAssignment)))
  S <-y[[poolAssignment$simple]]
  C <-y[[poolAssignment$complex]]
  B <- y[[poolAssignment$biomass]]
  
  dy <- rep(NA, length(y))
  
  if(verbose) cat('Uptake kinetics: ')
  if(grepl('^Monod$', rateFlags$uptake) | grepl('^MM$', rateFlags$uptake) ){
    if(verbose) cat('Monod\n')
    uptake <- parms['v_up']*S*B/(parms['km_up']+S)
  }else if(grepl('^revMM$', rateFlags$uptake)){
    if(verbose) cat('reverse Monod\n')
    uptake <- parms['v_up']*S*B/(parms['km_up']+B)
  }else{
    stop(sprintf('Bad uptake flag: %s',rateFlags$uptake) )
  }
  
  if('enzyme' %in% names(poolAssignment)){
    E <- y[[poolAssignment$enzyme]]
    Eproxy <- E
    Eturnover <- parms['turnover_e']*E
    
    if(verbose) cat('Enzyme production: ')
    if(grepl('^uptake$', rateFlags$prod)){
      if(verbose) cat('proptional to uptake\n')
      enzProd <- parms['enzProd']*uptake
    }else if(grepl('^biomass$', rateFlags$prod)){
      if(verbose) cat('proptional to biomass\n')
      enzProd <- parms['enzProd']*B
    }else if(grepl('^const$', rateFlags$prod)){
      if(verbose) cat('constant\n')
      enzProd <- parms['enzProd']
    }else{
      stop('Bad enzyme production flag.')
    }
    Ecost <- (1+parms['enzCost'])*enzProd
 
    dE <- enzProd - Eturnover
    dy[poolAssignment$enzyme] <- dE
  }else{
    #assume biomass acts as enzyme
    Eproxy <- B
    Eturnover <- 0
    Ecost <- 0
  }
  
  if(grepl('^MM$', rateFlags$enz)){
    enzCat <- parms['v_enz']*C*Eproxy/(parms['km_enz']+C)
  }else if(grepl('^revMM$', rateFlags$enz)){
    enzCat <- parms['v_enz']*C*Eproxy/(parms['km_enz']+Eproxy)
  }else if(grepl('^multi$', rateFlags$enz)){
    enzCat <- parms['v_enz']*C*Eproxy
  }else{
    stop('Bad enzyme flag.')
  }
  
  dC <- -enzCat + parms['turnover_b']*B + Eturnover
  dS <- enzCat - uptake
  dB <- parms['cue']*uptake - Ecost - parms['turnover_b']*B - parms['basal']*B
  
  if('dormant' %in% names(poolAssignment)){
    D <- y[[poolAssignment$dormant]]
    
    if(dB < 0 & dB < B){
      dD <- parms['dormancy_rate']*B
      if(dD-dB > B){
        dD <- B+dB
      }
      dB <- dB - dD
    }
    
    if(dB >= 0){
      dD <- -1*parms['dormancy_rate']*D
      dB <- dB + dD
    }
    dy[poolAssignment$dormant] <- dD
  }
  
  dy[poolAssignment$simple] <- dS
  dy[poolAssignment$complex] <- dC
  dy[poolAssignment$biomass] <- dB
  
  names(dy)[unlist(poolAssignment)] <- names(poolAssignment)
  
  return(list(dy))
}