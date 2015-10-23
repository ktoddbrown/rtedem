#' Change in carbon pools for a Simple-Complex-Biomass (SCB) decay model
#'
#' @param t numeric time (ignored)
#' @param y numeric carbon pools
#' @param parms list of named parameters
#' @param poolAssignment list of named pool indecies
#' @param rateFlags list of strings selecting rate options. Options are listed in defaults.
#' @param verbose logical flag to give useful debug statements
#' 
#' @return a list with the change in carbon for SCB models
#' @export
#'
#' @import assertthat
dC.SCBModel <- function(t, y, parms, 
                        poolAssignment = list(simple=1, complex=2, biomass=3),
                        rateFlags=list(enz=c('MM', 'revMM', 'multi')[1], 
                                       uptake=c('MM', 'revMM')[1]),
                        verbose=FALSE){
  assert_that(all(c('simple', 'complex', 'biomass') %in% names(poolAssignment)))
  assert_that(length(poolAssignment) == 3)
  S <-y[[poolAssignment$simple]]
  C <-y[[poolAssignment$complex]]
  B <- y[[poolAssignment$biomass]]
  
  if(verbose) {print(names(parms))}
  assert_that(all(c('v_enz', c('km_enz')[grepl('MM', rateFlags$enz)],
                    'v_up', 'km_up',
                    'cue', 'basal',
                    'turnover_b') %in% names(parms)))
  
  if(grepl('^MM$', rateFlags$enz)){
    enzCat <- parms$v_enz*C*B/(parms$km_enz+C)
  }else if(grepl('^revMM$', rateFlags$enz)){
    enzCat <- parms$v_enz*C*B/(parms$km_enz+B)
  }else if(grepl('^multi$', rateFlags$enz)){
    enzCat <- parms$v_enz*C*B
  }else{
    stop(sprintf('Bad enzyme flag: %s',rateFlags$uptake) )
  }
  
  if(grepl('^MM$', rateFlags$uptake)){
    uptake <- parms$v_up*S*B/(parms$km_up+S)
  }else if(grepl('^revMM$', rateFlags$uptake)){
    uptake <- parms$v_up*S*B/(parms$km_up+B)
  }else{
    stop(sprintf('Bad uptake flag: %s',rateFlags$uptake) )
  }
  
  dC <- -enzCat + parms$turnover_b*B
  dS <- enzCat - uptake
  dB <- parms$cue*uptake - parms$turnover_b*B - parms$basal*B
  
  dy <- list()
  dy[[poolAssignment$simple]] <- dS
  dy[[poolAssignment$complex]] <- dC
  dy[[poolAssignment$biomass]] <- dB
  return(dy)
}