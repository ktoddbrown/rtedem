#' Change in carbon pools for a Simple-Complex-Biomass decay model
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
dC.SCBModel <- function(t, y, parms, 
                        poolAssignment = list(simple=1, complex=2, biomass=3),
                        rateFlags=list(enz=c('MM', 'revMM', 'multi')[1], 
                                       uptake=c('MM', 'revMM')[1])){
  S <-y[[poolAssignment$simple]]
  C <-y[[poolAssignment$complex]]
  B <- y[[poolAssignment$biomass]]
  
  if(grepl('^MM$', rateFlags$enz)){
    enzyCat <- parms$v_enz*C*B/(parms$km_enz+C)
  }else if(grepl('^revMM$', rateFlags$enzStr)){
    enzyCat <- parms$v_enz*C*B/(parms$km_enz+B)
  }else if(grepl('^multi$', rateFlags$enzStr)){
    enzyCat <- parms$v_enz*C*B
  }else{
    stop('Bad enzyme flag.')
  }
  
  if(grepl('^MM$', rateFlags$uptake)){
    uptake <- parms$v_up*S*B/(parms$km_up+S)
  }else if(grepl('^revMM$', rateFlags$uptake)){
    uptake <- parms$v_up*S*B/(parms$km_up+B)
  }else{
    stop('Bad uptake flag.')
  }
  
  dC <- -enzCat + parms$turnover_b*B
  dS <- enzCat - uptake
  dB <- parms$cue*uptake - parms$turnover_b*B - parms$basal*B
  
  dy <- list()
  dy[[poolAssignment$simple]] <- dS
  dy[[poolAssignment$complex]] <- dC
  dy[[poolAssignment$enzyme]] <- dB
  return(dy)
}