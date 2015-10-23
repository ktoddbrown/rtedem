#' Create upper lower bounds for decomposition parameters
#'
#' @param modelBase a list of model parameters. See \code{publishedParmaeters.R} for details on data structure.
#' @param verbose boolean flag to help debug
#'
#' @return a list of data frames defining the upper and lower bounds of parameters
#' @export
#' @import plyr
#'
makeModelBounds <- function(modelBase=publishedParameters(), verbose=FALSE){

  assert_that(class(modelBase) %in% 'list')
  lapply(modelBase, FUN=function(xx){assert_that(all(c('tau', 'trans') %in% names(xx) )) })
    
  ans <- list()
  for(modelStr in names(modelBase)){
    numPool <- length(modelBase[[modelStr]]$tau)
    key <- data.frame(pool=1:numPool, tau=modelBase[[modelStr]]$tau)
    
    uniqueTau <- sort(unique(modelBase[[modelStr]]$tau))
    tauMax <- abs(diff(rev(uniqueTau)[1:2]))+rev(uniqueTau)[1]
    tauMids <- (uniqueTau[2:length(uniqueTau)]+uniqueTau[(2:length(uniqueTau))-1])/2
    
    values <- data.frame(tau=uniqueTau, 
                         min = c(0, tauMids),
                         max = c(tauMids, tauMax))
    
    turnovers <- merge(key, values, all=TRUE)
    turnovers <- turnovers[turnovers$pool,]
    
    parDef <- data.frame(name=c(paste('K', turnovers$pool, sep=''), as.character(modelBase[[modelStr]]$trans$name)),
                         true=c(-1*turnovers$tau, modelBase[[modelStr]]$trans$val),
                         max=c(-1*turnovers$min, rep(1, length(modelBase[[modelStr]]$trans$val))),
                         min=c(-1*turnovers$max, rep(0, length(modelBase[[modelStr]]$trans$val))))
    
    
    if(verbose){
      print(modelStr)
      print('turnover times:')
      print(diag(parDef$true[grepl('^K', parDef$name)]))
      print('transfer martrix:')
      temp <- rep(0, sum(grepl('^K', parDef$name))^2)
      temp[as.numeric(gsub('^A', '', parDef$name[grepl('^A', parDef$name)]))] <- parDef$true[grepl('^A', parDef$name)]
      print(matrix( temp, nrow=sum(grepl('^K', parDef$name))))
    }
    
    ans[[modelStr]] <- parDef
  }
  
  if(verbose){
    temp <- ldply(ans, function(x){data.frame(val=-1*x$true[grepl('^K', x$name)]/365,
                                              lower=-1*x$max[grepl('^K', x$name)]/365, 
                                              upper=-1*x$min[grepl('^K', x$name)]/365,
                                              pool=1:length(x$min[grepl('^K', x$name)]))})
    names(temp)[1] <- 'modelStr'
  }
  return(ans)
}