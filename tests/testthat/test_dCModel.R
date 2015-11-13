# Testing code for the RCMIP5 'dCModel.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code:
#   source("R/dCModel.R")
#   library(testthat)
#   test_file("tests/testthat/test_dCModel.R")

context("dCModel")

test_that('dCModel produces expected errors',{
  expect_error(dCModel(t=0, parms=c(a=1, b=2), reactionNetwork=data.frame(from=c('C1'), to='C2', reaction=c('a+c'))))
  expect_error(dCModel(t=0, parms=c(a=1, b=2), reactionNetwork=data.frame(from=c('C1'), to='C2', reaction=c('a+C3'))))
})

test_that('dCModel runs with and without factos', {
  reactionNetwork1 <- data.frame(from=c('C1', 'C1', 'C2', 'C2'), 
                                to=c(NA, 'C2', NA, 'C1'), 
                                reaction=c('1/tau1*(1-trans2)*C1', '1/tau1*trans2*C1', 
                                           '1/tau2*(1-trans3)*C2', '1/tau2*trans3*C2'),
                                type=c('decay', 'transfer', 'decay', 'transfer'),
                                stringsAsFactors=FALSE)
  reactionNetwork2 <- data.frame(from=c('C1', 'C1', 'C2', 'C2'), 
                                 to=c(NA, 'C2', NA, 'C1'), 
                                 reaction=c('1/tau1*(1-trans2)*C1', '1/tau1*trans2*C1', 
                                            '1/tau2*(1-trans3)*C2', '1/tau2*trans3*C2'),
                                 type=c('decay', 'transfer', 'decay', 'transfer'),
                                 stringsAsFactors=TRUE)
  y <- c(C1=1, C2=3)
  parms <- c(tau1=10, tau2=100, trans2=0.5, trans3=0.1)
  expect_equal(dCModel(t=0, y=y, parms=parms, reactionNetwork=reactionNetwork1),
               dCModel(t=0, y=y, parms=parms, reactionNetwork=reactionNetwork2))
})

test_that('dCModel reproduces a simple first order model', {
  expect_equal(dCModel(t=0), list(unlist(list(C1=-0.097, C2=0.020))))
  
  reactionNetwork <- data.frame(from=c('C1', 'C1', 'C2', 'C2'), 
                                to=c(NA, 'C2', NA, 'C1'), 
                                reaction=c('1/tau1*(1-trans2)*C1', '1/tau1*trans2*C1', 
                                           '1/tau2*(1-trans3)*C2', '1/tau2*trans3*C2'),
                                type=c('decay', 'transfer', 'decay', 'transfer'),
                                stringsAsFactors=FALSE)
  y <- c(C1=1, C2=3)
  parms <- c(tau1=10, tau2=100, trans2=0.5, trans3=0.1)
  
  decayMatrix <- matrix(c(-1/parms['tau1'], 1/parms['tau1']*parms['trans2'], 
                        1/parms['tau2']*parms['trans3'], -1/parms['tau2']), nrow=2)
  ans <- as.numeric(t(decayMatrix%*%matrix(y, nrow=2)))
  names(ans) <- c('C1', 'C2')
  
  expect_equal(list(ans),
                  dCModel(t=0, y=y, parms=parms, reactionNetwork=reactionNetwork))
})

test_that('dCModel produces expected errors',{

  expect_error(dCModel(parms=list(not=1, real=2), t=1, y=c(1,1,1,1)))
  expect_error(dCModel(parms=list(not=1, real=2), t=1, y=1))
  par <- unlist(list('v_enz'=0.2, 'km_enz'=10,
                     'v_up'=1, 'km_up'=2,
                     'cue'=0.5, 'basal' = 0.01,
                     'turnover_b'=0.5, 'turnover_e'=0.1))
  expect_error(dCModel(parms=par, t=1, y=1))
  
  par <- unlist(list('v_enz'=0.2, 'km_enz'=10, turnover_e=0.1))[1:2]
  y0 <- unlist(list(simple=1, complex=2, enzyme=3))
  poolAssignment <- list(simple=1, complex=2, enzyme=3)
  expect_error(dCModel(parms=par, y=y0, rateFlags=list(enz='MM'), poolAssignment=poolAssignment))
})

test_that('dC.biomassModel returns correct enzyme kinetics',{
  par <- unlist(list('v_enz'=0.2, 'km_enz'=10, turnover_e=0.1))
  y0 <- unlist(list(simple=1, complex=2, enzyme=3))
  renet <- data.frame(from=c('complex', 'enzyme'),
                          to=c('simple', 'complex'),
                          reaction=c('complex*enzyme*v_enz/(km_enz+complex)',
                                            'turnover_e*enzyme'), stringsAsFactors=FALSE)
  
  expect_equal(unlist(dCModel(t=0, y=y0, parms=par,reactionNetwor=renet )),
               unlist(list(simple=as.numeric(par['v_enz']*y0['complex']*y0['enzyme']/(par['km_enz']+y0['complex'])), 
                           complex=as.numeric(par['turnover_e']*y0['enzyme']-par['v_enz']*y0['complex']*y0['enzyme']/(par['km_enz']+y0['complex'])), 
                           enzyme=as.numeric(-par['turnover_e']*y0['enzyme']))))
  
  renet <- data.frame(from=c('complex', 'enzyme'),
                      to=c('simple', 'complex'),
                      reaction=c('complex*enzyme*v_enz/(km_enz+enzyme)',
                                 'turnover_e*enzyme'), stringsAsFactors=FALSE)
  expect_equal(unlist(dCModel(t=0, y=y0, parms=par,reactionNetwor=renet )),
               unlist(list(simple=as.numeric(par['v_enz']*y0['complex']*y0['enzyme']/(par['km_enz']+y0['enzyme'])), 
                           complex=as.numeric(par['turnover_e']*y0['enzyme']-par['v_enz']*y0['complex']*y0['enzyme']/(par['km_enz']+y0['enzyme'])), 
                           enzyme=as.numeric(-par['turnover_e']*y0['enzyme']))))

  par <- unlist(list('v_enz'=0.2, turnover_e=0.1))
  renet <- data.frame(from=c('complex', 'enzyme'),
                      to=c('simple', 'complex'),
                      reaction=c('complex*enzyme*v_enz',
                                 'turnover_e*enzyme'), stringsAsFactors=FALSE)
  expect_equal(unlist(dCModel(t=0, y=y0, parms=par,reactionNetwor=renet )),
               unlist(list(simple=as.numeric(par['v_enz']*y0['complex']*y0['enzyme']), 
                           complex=as.numeric(par['turnover_e']*y0['enzyme']-par['v_enz']*y0['complex']*y0['enzyme']), 
                           enzyme=as.numeric(-par['turnover_e']*y0['enzyme']))))
})
