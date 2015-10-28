#' Change in carbon pools for a Simple-Complex-Enzyme-Active-Dormant (SCEAD) decay model
#'
#' @param t numeric time (ignored)
#' @param y numeric carbon pools
#' @param parms list of named parameters
#' @param poolAssignment list of named pool indecies
#' @param rateFlags list of strings selecting rate options. Options are listed in defaults.
#' @param verbose logical flag to give useful debug statements
#' 
#' @return a list with the change in carbon in SCEB models
#' @export
#' @import assertthat
dC.SCEADModel <- function(t, y, parms, 
                         poolAssignment = list(simple=1, complex=2, enzyme=3, biomass=4, dormant=5),
                         rateFlags=list(enz=c('MM', 'revMM', 'multi')[1], 
                                        uptake='Monod', 
                                        prod=c('uptake', 'biomass', 'const')[1]),
                         verbose=FALSE){

  assert_that(all(c('simple', 'complex', 'enzyme', 'biomass', 'dormant') %in% names(poolAssignment)))
  assert_that(length(y) == 5)
  S <-y[[poolAssignment$simple]]
  C <-y[[poolAssignment$complex]]
  E <- y[[poolAssignment$enzyme]]
  B <- y[[poolAssignment$biomass]]
  D <- y[[poolAssignment$dormant]]

  if(verbose) {print(names(parms))}
  assert_that(all(c('v_enz', c('km_enz')[grepl('MM', rateFlags$enz)],
                 'v_up', 'km_up',
                 'enzProd',
                 'enzCost', 'cue', 'basal',
                 'turnover_b', 'turnover_e', 'dormancy_rate') %in% names(parms)))
  
  if(grepl('^MM$', rateFlags$enz)){
    enzCat <- parms['v_enz']*C*E/(parms['km_enz']+C)
  }else if(grepl('^revMM$', rateFlags$enz)){
    enzCat <- parms['v_enz']*C*E/(parms['km_enz']+E)
  }else if(grepl('^multi$', rateFlags$enz)){
    enzCat <- parms['v_enz']*C*E
  }else{
    stop('Bad enzyme flag.')
  }
  
  if(grepl('^Monod$', rateFlags$uptake)){
    uptake <- parms['v_up']*S*B/(parms['km_up']+S)
  }else{
    stop(sprintf('Bad uptake flag: %s',rateFlags$uptake) )
  }
  
  if(grepl('^uptake$', rateFlags$prod)){
    enzProd <- parms['enzProd']*uptake
  }else if(grepl('^biomass$', rateFlags$prod)){
    enzProd <- parms['enzProd']*B
  }else if(grepl('^const$', rateFlags$prod)){
    enzProd <- parms['enzProd']
  }else{
    stop('Bad enzyme production flag.')
  }
  
  dC <- -enzCat + parms['turnover_b']*B + parms['turnover_e']*E
  dS <- enzCat - uptake
  dB <- parms['cue']*uptake - (1+parms['enzCost'])*enzProd - parms['turnover_b']*B - parms['basal']*B
  dE <- enzProd - parms['turnover_e']*E
  
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
  
  dy <- rep(NA, length(y))
  dy[poolAssignment$simple] <- dS
  dy[poolAssignment$complex] <- dC
  dy[poolAssignment$enzyme] <- dE
  dy[poolAssignment$biomass] <- dB
  dy[poolAssignment$dormant] <- dD
  names(dy)[unlist(poolAssignment)] <- names(poolAssignment)
  
  return(list(dy))
}