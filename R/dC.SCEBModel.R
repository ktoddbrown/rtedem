#' Change in carbon pools for a Simple-Complex-Enzyme-Biomass decay model
#'
#' @param t numeric time (ignored)
#' @param y numeric carbon pools
#' @param parms list of named parameters
#' @param poolAssignment list of named pool indecies
#' @param rateFlags list of strings selecting rate options. Options are listed in defaults.
#'
#' @return
#' @export
#'
dC.SCEBModel <- function(t, y, parms, 
                         poolAssignment = list(simple=1, complex=2, enzyme=3, biomass=4),
                         rateFlags=list(enz=c('MM', 'revMM', 'multi')[1], 
                                        uptake='Monod', 
                                        prod=c('uptake', 'biomass', 'const')[1])){
  S <-y[[poolAssignment$simple]]
  C <-y[[poolAssignment$complex]]
  E <- y[[poolAssignment$enzyme]]
  B <- y[[poolAssignment$biomass]]
  
  if(grepl('^MM$', rateFlags$enz)){
    enzyCat <- parms$v_enz*C*E/(parms$km_enz+C)
  }else if(grepl('^revMM$', rateFlags$enzStr)){
    enzyCat <- parms$v_enz*C*E/(parms$km_enz+E)
  }else if(grepl('^multi$', rateFlags$enzStr)){
    enzyCat <- parms$v_enz*C*E
  }else{
    stop('Bad enzyme flag.')
  }
  
  if(grepl('^Monod$', rateFlags$uptake)){
    uptake <- parms$v_up*S*B/(parms$km_up+S)
  }else{
    stop('Bad uptake flag.')
  }
  
  if(grepl('^uptake$', rateFlags$prod)){
    enzProd <- parms$p*uptake
  }else if(grepl('^biomass$', rateFlags$prod)){
    enzProd <- parms$p*B
  }else if(grepl('^const$', rateFlags$prod)){
    enzProd <- parms$p
  }else{
    stop('Bad enzyme production flag.')
  }
  
  dC <- -enzCat + parms$turnover_b*B + parms$turnover_e*E
  dS <- enzCat - uptake
  dB <- parms$cue*uptake - (1+parms$enzCost)*enzProd - parms$turnover_b*B - parms$basal*B
  dE <- enzProd - parms$turnover_e*E
  
  dy <- list()
  dy[[poolAssignment$simple]] <- dS
  dy[[poolAssignment$complex]] <- dC
  dy[[poolAssignment$enzyme]] <- dB
  dy[[poolAssignment$biomass]] <- dE
  return(dy)
}