dC.guildModel <- function(t, y=c(C1=1, C2=3), 
                          parms=c(k1=0.1, k2=0.01, trans2=0.5, trans3=0.1), 
                          reactionNetwork = data.frame(from=c('C1', 'C1', 'C2', 'C2'), 
                                                       to=c(NA, 'C2', NA, 'C1'), 
                                                       reaction=c('k1*(1-trans2)*C1', 'k1*trans2*C1', 'k2*(1-trans3)*C2', 'k2*trans3*C2'),
                                                       stringsAsFactors=FALSE), 
                          verbose=FALSE){
  
  allStr <- paste0(reactionNetwork$reaction, collapse='+')
  usedVars <- sort(unique(unlist(regmatches(allStr, gregexpr('[a-zA-Z]\\w*', allStr, perl=TRUE)))))
  if(verbose)cat('usedVars: [', usedVars, ']\n')
  if(verbose){cat('parms:\n'); print(parms)}
  #if(verbose)cat(sort(names(y), names(parms))
  if(verbose)cat('declared vars:[', sort(unique(c(names(y), names(parms)))), ']\n')
  assert_that(identical(usedVars, sort(unique(c(names(y), names(parms))))))
  
  temp <- ddply(reactionNetwork, c('from', 'to', 'reaction'), summarize, value = eval(parse(text = reaction), envir=c(as.list(y), parms)))
  
  dC <- data.frame()
  for(idStr in unique(c(temp$to, temp$from))){
    dC <- rbind.fill(dC, data.frame(id=idStr, 
                                    value=sum(temp$value[temp$to %in% idStr]) -
                                          sum(temp$value[temp$from %in% idStr])))
  }
  dy <- dC$value[!is.na(dC$id)]
  names(dy) <- dC$id[!is.na(dC$id)]
  dy <- dy[names(y)]
  return(list(dy))
}