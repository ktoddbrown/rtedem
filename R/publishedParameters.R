#' Create list with published parameters
#'
#' @param nameArr an array of characers naming the models to be pulled. Can be NULL if no subset is desired.
#'
#' @return A list of lists describing the parameters and citations for models. Decomposition pools are defined as pools which respire carbon dioxide and are not vegitation pools. Tau is in days and trans are fractional transfers to other carbon pools indexed as column matrix.
#' @export
#' @import assertthat
publishedParameters <- function(nameArr = NULL){
 
  assertthat::is.string(nameArr)
  
   ans <- list(
    #YASSO07, Tuomi et al 2009, 5 pool decomposition
    YASSO07 = list(tau=(1/c(0.66, 4.3, 0.35, 0.22, 3.3e-3))*365,
                 trans=data.frame(name=paste('A', c(2,3,4,5,6,8,9,10,11,12,14,15,16,17,18,20), sep=''),
                                  val = c(      0.34, 0,    0,    0.04, 
                                                0.32,       0,    0,    0.04,
                                                0.01, 0,          0.92, 0.04,
                                                0.93, 0,    0.01,       0.04)),
                 type=c('litter-acid', 'litter-water', 'litter-nonpolar', 'litter-other', 'soil'),
                 citation='http://dx.doi.org/10.1016/j.ecolmodel.2009.05.016'),
    #CLM, Thornton and Rosenbloom 2005, 6 pool decomposition
    ##... the course wordy debris pool is ignored because no CO2 respired
    CLM = list(tau = 1/c(0.7, 0.07, 0.014, 0.07, 0.014, 0.0005),
               trans = data.frame(name=c('A4', 'A11', 'A18', 'A23', 'A30'),
                                  val=c(1-0.39, 1-0.55, 1-0.29, 1-0.28, 1-0.46)),
               type=c('litter-waterAlcohol', 'litter-acid', 'litter-other', 'soil-1', 'soil-2', 'soil-3'),
               citation='http://dx.doi.org/10.1016/j.ecolmodel.2005.04.008'),
    #TECO, Xu et al 2006, 5 pool decomposition
    TECO =list(tau = 1/c(1.7e-2, 5.10e-3, 1.04e-3, 1.70e-4, 5.25e-6),
               trans = data.frame(name=c('A3', 'A8', 'A9', 'A14', 'A15', 'A18', 'A20', 'A23'),
                                  val=c(0.45, 0.275, 0.275, 0.296, 0.004, 0.42, 0.03, 0.45)),
               type=c('litter-metabolic', 'litter-structure', 'soil-microbe', 'soil-slow', 'soil-passive'),
               citation='http://dx.doi.org/10.1029/2005GB002468'),
    #CENTURY, Parton et al 1987, 1988 6 pool decomposition
    #Let clay+silt fraction T=0.5; structural lignin fraction A=0.1
    CENTURY=list(tau = c(0.5, 3, 3, 1.5, 25, 1e3)*365,
                 trans=data.frame(name=paste('A', c(4,10,16,28, 34, 11, 17, 23, 24, 30), sep=''),
                                  val=c(0.45, (1-0.1)*0.55, (1-0.1)*0.45,           0.42, 0.45,
                                        0.1*0.7,      0.1*0.7, 0.146*0.68*0.5, 
                                        0.004,     0.03)),
                 type=c('litter-metabolic', 'litter-surfaceStructure', 'litter-belowStructure', 
                        'soil-active', 'soil-slow', 'soil-passive'),
                 citation='http://dx.doi.org/10.2136/sssaj1987.03615995005100050015x')
    
    
    )##end ans <- list()
  
  if(!is.null(nameArr)){
    ans <- ans[[intersect(nameArr, names(ans))]]
  }
  
  return(ans)
}