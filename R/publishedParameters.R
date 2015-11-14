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
    YASSO07 = list(par=c(tau1=1/0.66*365, tau2=1/4.3*365, tau3=1/0.35*365, tau4=1/0.22*365, tau5=1/3.3e-3*365, transC1C2=0.34, transC1C5=0.04, transC2C1=0.32, transC2C5=0.04, transC3C1=0.01, transC3C4=0.92, transC3C5=0.04, transC4C1=0.93, transC4C3=0.01, transC4C5=0.04),
                  reactionNetwork=data.frame(
                    from=c('C1', 'C2', 'C3', 'C4', 'C5', 'C1', 'C1', 'C2', 'C2', 'C3', 'C3', 'C3', 'C4', 'C4', 'C4'),
                    to = c('CO2', 'CO2', 'CO2', 'CO2', 'CO2', 'C2', 'C5', 'C1', 'C5', 'C1', 'C4', 'C5', 'C1', 'C3', 'C5'),
                    reaction=c('C1/tau1*(1-transC1C2-transC1C5)',
                               'C2/tau2*(1-transC2C1-transC2C5)',
                               'C3/tau3*(1-transC3C1-transC3C4-transC3C5)',
                               'C4/tau4*(1-transC4C1-transC4C3-transC4C5)',
                               'C5/tau5',
                               'C1/tau1*transC1C2',
                               'C1/tau1*transC1C5',
                               'C2/tau2*transC2C1',
                               'C2/tau2*transC2C5',
                               'C3/tau3*transC3C1',
                               'C3/tau3*transC3C4',
                               'C3/tau3*transC3C5',
                               'C4/tau4*transC4C1',
                               'C4/tau4*transC4C3',
                               'C4/tau4*transC4C5'), 
                    stringsAsFactors=FALSE
                  ),
                  type=c('litter-acid', 'litter-water', 'litter-nonpolar', 'litter-other', 'soil'),
                  citation='http://dx.doi.org/10.1016/j.ecolmodel.2009.05.016'),
    CLM = list(type=c('litter-waterAlcohol', 'litter-acid', 'litter-other', 'soil-1', 'soil-2', 'soil-3'),
              citation='http://dx.doi.org/10.1016/j.ecolmodel.2005.04.008',
              par = c(tau1=1/0.7, tau2=1/0.07, tau3=1/0.014, tau4=1/0.07, tau5=1/0.014, tau6=1/0.0005, transC1C4=1-0.39, transC2C5=1-0.55, transC3C6=1-0.29, transC4C5=1-0.28, transC5C6=1-0.46),
              reactionNetwork=data.frame(from=c('C1', 'C1', 'C2', 'C2', 'C3', 'C3', 'C4', 'C4', 'C5', 'C5', 'C6'),
                                  to=c('CO2', 'C4', 'CO2', 'C5', 'CO2', 'C6', 'CO2', 'C5', 'CO2', 'C6', 'CO2'),
                                  reaction=c('C1/tau1*(1-transC1C4)', 'C1/tau1*transC1C4',
                                             'C2/tau2*(1-transC2C5)', 'C2/tau2*transC2C5',
                                             'C3/tau3*(1-transC3C6)', 'C3/tau3*transC3C6',
                                             'C4/tau4*(1-transC4C5)', 'C4/tau4*transC4C5',
                                             'C5/tau5*(1-transC5C6)', 'C5/tau5*transC5C6',
                                             'C6/tau6'),
                                  stringsAsFactors=FALSE)),
    TECO =list(type=c('litter-metabolic', 'litter-structure', 'soil-microbe', 'soil-slow', 'soil-passive'),
               citation='http://dx.doi.org/10.1029/2005GB002468',
               par = c(tau1=1/1.7e-2, tau2=1/5.10e-3, tau3=1/1.04e-3, tau4=1/1.70e-4, tau5=1/5.25e-6, transC1C3=0.45, transC2C3=0.275, transC2C4=0.275, transC3C4=0.296, transC3C5=0.004, transC4C3=0.42, transC4C5=0.03, transC5C3=0.45),
               reactionNetwork=data.frame(from=c('C1', 'C1', 'C2', 'C2', 'C2', 'C3', 'C3', 'C3', 'C4', 'C4', 'C4', 'C5', 'C5'),
                                   to=c('C3', 'CO2', 'C3', 'C4', 'CO2', 'C4', 'C5', 'CO2', 'C3', 'C5', 'CO2', 'C3', 'CO2'),
                                   reaction=c('C1/tau1*transC1C3', 'C1/tau1*(1-transC1C3)',
               'C2/tau2*transC2C3', 'C2/tau2*transC2C4', 'C2/tau2*(1-transC2C3-transC2C4)',
               'C3/tau3*transC3C4', 'C3/tau3*transC3C5', 'C3/tau3*(1-transC3C4-transC3C5)',
               'C4/tau4*transC4C3', 'C4/tau4*transC4C5', 'C4/tau4*(1-transC4C3-transC4C5)',
               'C5/tau5*transC5C3', 'C5/tau5*(1-transC5C3)'))),
    #CENTURY, Parton et al 1987, 1988 6 pool decomposition
    #Let clay+silt fraction T=0.5; structural lignin fraction A=0.1
    CENTURY=list(type=c('litter-metabolic', 'litter-surfaceStructure', 'litter-belowStructure', 
                        'soil-active', 'soil-slow', 'soil-passive'),
                 citation='http://dx.doi.org/10.2136/sssaj1987.03615995005100050015x',
                 par = c(tau1=0.5*365, tau2=3*365, tau3=3*365, tau4=1.5*365, tau5=25*365, tau6=1e3*365, T=0.5, A=0.1, transC1C4=0.45, transC2C4=1-0.45, transC2C5=1-0.3, transC3C4=1-0.55, transC3C5=1-0.3, transC4C5=1-0.85-0.004, transC4C6=0.004, transC5C4=0.042,  transC5C6=0.03, transC6C4=1-0.55),
                 reactionNetwork=data.frame(from=c('C1', 'C1', 'C2', 'C2', 'C2', 'C3', 'C3', 'C3', 'C4', 'C4', 'C4', 'C5', 'C5', 'C5', 'C6', 'C6'),
                                            to=c('C4', 'CO2', 'C4', 'C5', 'CO2', 'C4', 'C5', 'CO2', 'C5', 'C6', 'CO2', 'C4', 'C6', 'CO2', 'C4', 'CO2'),
               reaction=c('C1/tau1*transC1C4', 'C1/tau1*(1-transC1C4)',
'C2/tau2*(1-A)*transC2C4','C2/tau2*A*transC2C5', 'C2/tau2*(1-(1-A)*transC2C4-A*transC2C5)',
'C3/tau3*(1-A)*transC3C4','C3/tau3*A*transC3C5', 'C3/tau3*(1-(1-A)*transC3C4-A*transC3C5)',
'C4/tau4*(transC4C5+0.68*T)','C4/tau4*transC4C6', 'C4/tau4*(1-(transC4C5+0.68*T)-transC4C6)',
'C5/tau5*transC5C4', 'C5/tau5*transC5C6', 'C5/tau5*(1-transC5C4-transC5C6)',
'C6/tau6*transC6C4', 'C6/tau6*(1-transC6C4)'))),
  
   #Wang, G., Post, W. M. and Mayes, M. A.: Development of microbial-enzyme-mediated decomposition model parameters through steady-state and dynamic analyses, Ecol. Appl., 23(1), 255–272, doi:10.1890/12-0681.1, 2013.
    MEND = list(type=c('biomass-B', 'dissolved-D', 'particulate-P', 'absorbed-Q', 'mineralAssoc-M', 'p-enzyme-EP', 'm-enzmye-EM'),
               citation='http://dx.doi.org/10.1890/12-0681.1',
               y0Units='mg C/g soil',
               y0 = c(B=2, D=1, P=10, Q=0.1, M=5, EP=1e-5, EM=1e-5, CO2=0),
               par=c(m_R=2.8e-4, 
                     E_C=0.47, 
                     f_D=0.5, g_D=0.5, 
                     r_EP=1e-3, r_EM=1e-3, 
                     p_EP=1e-2, p_EM=1e-2,
                     Q_max=1.7,
                     K_BA=6,
                     K_des=1e-3,
                     K_D=0.26,
                     V_D=5e-4,
                     V_P=2.5,
                     K_P=50,
                     V_M=1,
                     K_M=250),
               reactionNetwork=data.frame(
                 type=c('uptake', 'PtoM', 'PtoD', 'MtoD', 'resp_growth', 'resp_maint', 'absorpt', 'desorpt', 'death_P', 'death_D', 'enz_prod_EP', 'enz_prod_EM', 'enz_turnover_EP', 'enz_turnover_EM'),
                 from=c('D', 'P', 'P', 'M',  'B',   'B',  'D', 'Q', 'B', 'B', 'B', 'B', 'EP', 'EM' ),
                 to = c('B', 'M', 'D', 'D', 'CO2', 'CO2', 'Q', 'D', 'P', 'D', 'EP', 'EM', 'CO2', 'CO2' ),
                 reaction=c('1/E_C*(V_D+m_R)*D*B/(K_D+D)', #F1 eq9 uptake
                            '(1-f_D)*V_P*EP*P/(K_P+P)', #F2mod eq10 PtoM
                            '(f_D)*V_P*EP*P/(K_P+P)', #F2mod eq10, PtoD
                            'V_M*EM*M/(K_M+M)', #F3 eq11, MtoD
                            '(1/E_C-1)*V_D*B*D/(K_D+D)', #F4 eq12 resp_growth,
                            '(1/E_C-1)*m_R*B*D/(K_D+D)', #F5 eq13 maint_resp
                            'K_des*K_BA*(-Q/Q_max)*D', #F6 eq14 absorpt
                            'K_des*Q/Q_max', #F7 eq14 desorpt
                            '(1-g_D)*(1-p_EP-p_EM)*m_R*B', #F8mod eq16 death_P
                            'g_D*(1-p_EP-p_EM)*m_R*B', #F8mod eq16 death_D
                            'p_EP*m_R*B', #F9EP
                            'p_EM*m_R*B', #F9EM
                            'r_EP*EP',
                            'r_EM*EM'),
                 stringsAsFactors=FALSE)
  )
  )
  
  
   ansOLD <- list(
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
   #dCModel(t=0, y=c(C1=1, C2=1, C3=1, C4=3, C5=3), parms=ans_new$YASSO07$par, reactionNetwork=ans_new$YASSO07$reactionNetwork)
   #par <- c(temp$YASSO07$tau, temp$YASSO07$trans$val)
   #names(par) <- c(sprintf('tau%d', 1:length(temp$YASSO07$tau)), as.character(temp$YASSO07$trans$name))
   #dC.firstOrderModel(t=0, y=c(C1=1, C2=1, C3=1, C4=3, C5=3), parms=par)
  return(ans)
}