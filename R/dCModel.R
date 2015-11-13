#' Generalized decomposition model
#'
#' @param t numeric for timepoint
#' @param y named array pools
#' @param parms named array of parameters 
#' @param reactionNetwork data.frame with from, to identifying the pools named in y which the flux is leaving and entering (NA is valid to represent transfers outside the system), reaction is a string which will be evaluated in the context of the named pools and parameters passed above.
#' @param verbose boolean flag for useful debugging output
#'
#' @return a named array with the change in pools
#' @export
#' @import plyr
dCModel <- function(t, y=c(C1=1, C2=3), 
                          parms=unlist(list(k1=0.1, k2=0.01, trans2=0.5, trans3=0.1)), 
                          reactionNetwork = data.frame(from=c('C1', 'C1', 'C2', 'C2'), 
                                                       to=c(NA, 'C2', NA, 'C1'), 
                                                       reaction=c('k1*(1-trans2)*C1', 'k1*trans2*C1', 'k2*(1-trans3)*C2', 'k2*trans3*C2'),
                                                       stringsAsFactors=FALSE), 
                          verbose=FALSE){
  
  #make sure there are no factors, everything is a character
  reactionNetwork[] <- lapply(reactionNetwork, as.character)
  allStr <- paste0(reactionNetwork$reaction, collapse='+')
  usedVars <- sort(unique(c(names(y), unlist(regmatches(allStr, gregexpr('[a-zA-Z]\\w*', allStr, perl=TRUE))))))
  if(verbose)cat('usedVars: [', usedVars, ']\n')
  if(verbose){cat('parms:\n'); print(parms)}
  #if(verbose)cat(sort(names(y), names(parms))
  if(verbose)cat('declared vars:[', sort(unique(c(names(y), names(parms)))), ']\n')
  assert_that(all(usedVars %in% unique(c(names(y), names(parms)))))
  
  if(verbose) {cat('evalEnv:\n'); print(as.list(c(y, parms, t=t)))}
  temp <- ddply(reactionNetwork, c('from', 'to', 'reaction'), function(xx, evalEnv=as.list(c(y, parms))){ data.frame(value = eval(parse(text = xx$reaction), envir=evalEnv))})
  tempTo <- temp[,c('to', 'reaction', 'value')]
  tempFrom <- temp[,c('from', 'reaction', 'value')]
  tempFrom$value <- temp$value*-1
  
  names(tempTo)[1] <- c('pool')
  names(tempFrom)[1] <- c('pool')
  
  dC <- ddply(rbind(tempTo, tempFrom), c('pool'), summarize, value=sum(value))
  
  if(verbose) {cat('temp:\n'); print(temp)}
  
#   dC <- data.frame()
#   for(idStr in unique(c(temp$to, temp$from))){
#     dC <- rbind.fill(dC, data.frame(id=idStr, 
#                                     value=sum(temp$value[temp$to %in% idStr]) -
#                                           sum(temp$value[temp$from %in% idStr])))
#   }
  assert_that(abs(sum(dC$value)) < sum(y)*1e-8)
  if(verbose) {cat('dC:\n'); print(dC); cat('sum(dC$value): ', sum(dC$value))}
  dy <- dC$value[!is.na(dC$pool)]
  names(dy) <- dC$pool[!is.na(dC$pool)]
  dy <- dy[names(y)]
  return(list(dy))
}