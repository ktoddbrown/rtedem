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
                         poolAssignment = list(simple=1, complex=2, enzyme=3, biomass=4, dormant=5, protect_S=6, protect_C=7),
                         rateFlags=list(enz=c('MM', 'revMM', 'multi')[1], 
                                        uptake=c('Monod', 'MM', 'revMM')[1], 
                                        prod=c('uptake', 'biomass', 'const')[1],
                                        sorb=c('carbonCapacity')[1]),
                         verbose=FALSE){

  
  assert_that(all(names(poolAssignment) %in% c('simple', 'complex', 'enzyme', 'biomass', 'dormant', 'protect_S', 'protect_C')))
  cat()
  assert_that(ifelse(all(c('protect_S', 'protect_C') %in% names(poolAssignment)), ('sorb' %in% names(rateFlags)), TRUE))
  assert_that(length(y) >= max(unlist(poolAssignment)))
  if(verbose) {print(names(parms))}
  assert_that(all(c('v_enz', c('km_enz')[grepl('MM', rateFlags$enz)],
                    'v_up', 'km_up','cue', 'basal', 'turnover_b', 
                    c('enzProd', 'enzCost', 'turnover_e')['enzyme' %in% names(poolAssignment)],
                    c('dormancy_rate')['dormant' %in% names(poolAssignment)],
                    c('sorb_rate', 'desorb_rate', 'carbonCapacity')[all(c('protect_S', 'protect_C') %in% names(poolAssignment))]) %in% names(parms)))

  assert_that(all(c('simple', 'complex', 'biomass') %in% names(poolAssignment)))
  S <-y[[poolAssignment$simple]]
  C <-y[[poolAssignment$complex]]
  B <- y[[poolAssignment$biomass]]
  
  dy <- rep(NA, length(y))

  if(all(c('protect_S', 'protect_C') %in% names(poolAssignment))){
    if(verbose) cat('(De)sorbtion kinetics:')
    if(grepl('^carryCapacity$', rateFlags$sorb)){
      P_S <- y[[poolAssignment$protect_S]]
      P_C <- y[[poolAssignment$protect_C]]
      
      dP_S <- S*parms['sorb_rate']*(1-(P_S+P_C)/parms['carbonCapacity']) - 
              P_S*parms['desorb_rate']
      
      dP_C <- C*parms['sorb_rate']*(1-(P_S+P_C)/parms['carbonCapacity']) - 
              P_C*parms['desorb_rate']
        
      dy[[poolAssignment$protect_C]] <- dP_C
      dy[[poolAssignment$protect_S]] <- dP_S
      
      dC <- -dP_C
      dS <- -dP_S
    }else{
      stop(sprintf('Bad sorb flag: %s',rateFlags$sorb) )
    }
  }else{
    dC <- 0
    dS <- 0
  }
  
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
  
  dC <- dC - enzCat + parms['turnover_b']*B + Eturnover
  dS <- dS + enzCat - uptake
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