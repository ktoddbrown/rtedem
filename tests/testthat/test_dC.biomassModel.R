# Testing code for the RCMIP5 'dC.biomassModel.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code:
#   source("R/dC.biomassModel.R")
#   library(testthat)
#   test_file("tests/testthat/test_dC.biomassModel.R")

context("dC.biomassModel")

test_that('dCbiomassModel produces expected errors',{
  expect_error(dC.biomassModel())
  expect_error(dC.biomassModel(parms=list(not=1, real=2), t=1, y=c(1,1,1,1)))
  expect_error(dC.biomassModel(parms=list(not=1, real=2), t=1, y=1))
  par <- unlist(list('v_enz'=0.2, 'km_enz'=10,
              'v_up'=1, 'km_up'=2,
              'cue'=0.5, 'basal' = 0.01,
              'turnover_b'=0.5, 'turnover_e'=0.1))
  expect_error(dC.biomassModel(parms=par, t=1, y=1))
  
  par <- unlist(list('v_enz'=0.2, 'km_enz'=10, turnover_e=0.1))[1:2]
  y0 <- unlist(list(simple=1, complex=2, enzyme=3))
  poolAssignment <- list(simple=1, complex=2, enzyme=3)
  expect_error(dC.biomassModel(parms=par, y=y0, rateFlags=list(enz='MM'), poolAssignment=poolAssignment))
})

test_that('dC.biomassModel returns correct enzyme kinetics',{
  par <- unlist(list('v_enz'=0.2, 'km_enz'=10, turnover_e=0.1))
  y0 <- unlist(list(simple=1, complex=2, enzyme=3))
  poolAssignment <- list(simple=1, complex=2, enzyme=3)
  
  expect_equal(unlist(dC.biomassModel(parms=par, y=y0, rateFlags=list(enz='MM'), poolAssignment=poolAssignment)),
               unlist(list(simple=as.numeric(par['v_enz']*y0['complex']*y0['enzyme']/(par['km_enz']+y0['complex'])), 
                 complex=as.numeric(par['turnover_e']*y0['enzyme']-par['v_enz']*y0['complex']*y0['enzyme']/(par['km_enz']+y0['complex'])), 
                 enzyme=as.numeric(-par['turnover_e']*y0['enzyme']))))
  expect_equal(unlist(dC.biomassModel(parms=par, y=y0, rateFlags=list(enz='revMM'), poolAssignment=poolAssignment)),
               unlist(list(simple=as.numeric(par['v_enz']*y0['complex']*y0['enzyme']/(par['km_enz']+y0['enzyme'])), 
                           complex=as.numeric(par['turnover_e']*y0['enzyme']-par['v_enz']*y0['complex']*y0['enzyme']/(par['km_enz']+y0['enzyme'])), 
                           enzyme=as.numeric(-par['turnover_e']*y0['enzyme']))))
  expect_equal(unlist(dC.biomassModel(parms=par, y=y0, rateFlags=list(enz='multi'), poolAssignment=poolAssignment)),
               unlist(list(simple=as.numeric(par['v_enz']*y0['complex']*y0['enzyme']), 
                           complex=as.numeric(par['turnover_e']*y0['enzyme']-par['v_enz']*y0['complex']*y0['enzyme']), 
                           enzyme=as.numeric(-par['turnover_e']*y0['enzyme']))))
})

test_that('dC.biomassModel returns correct biomass growth and basal resp',{
  par <- unlist(list('v_up' = 0.2, 'km_up' = 10, basal=0.01, cue=0.8))
  y0 <- unlist(list(simple=2, biomass=1))
  poolAssignment <- list(simple=1, biomass=2)
  expect_equal(unlist(dC.biomassModel(parms=par, y=y0, rateFlags=list(uptake='MM'), poolAssignment=poolAssignment)),
               unlist(list(simple=as.numeric(-par['v_up']*y0['biomass']*y0['simple']/(par['km_up']+y0['simple'])), 
                          biomass=as.numeric(par['cue']*par['v_up']*y0['biomass']*y0['simple']/(par['km_up']+y0['simple'])
                                              -(par['basal'])*y0['biomass']))))
  expect_equal(unlist(dC.biomassModel(parms=par, y=y0, rateFlags=list(uptake='Monod'), poolAssignment=poolAssignment)),
               unlist(list(simple=as.numeric(-par['v_up']*y0['biomass']*y0['simple']/(par['km_up']+y0['simple'])), 
                           biomass=as.numeric(par['cue']*par['v_up']*y0['biomass']*y0['simple']/(par['km_up']+y0['simple'])
                                              -(par['basal'])*y0['biomass']))))
  expect_equal(unlist(dC.biomassModel(parms=par, y=y0, rateFlags=list(uptake='revMM'), poolAssignment=poolAssignment)),
               unlist(list(simple=as.numeric(-par['v_up']*y0['biomass']*y0['simple']/(par['km_up']+y0['biomass'])), 
                           biomass=as.numeric(par['cue']*par['v_up']*y0['biomass']*y0['simple']/(par['km_up']+y0['biomass'])
                                              -(par['basal'])*y0['biomass']))))
  
})

test_that('dC.biomassModel returns correct turnovers of biomass and enzyme pools',{
  parms <- unlist(list(turnover_e=0.2))
  y0 <- unlist(list(complex=0, enzyme=2))
  poolAssignment <- list(complex=1, enzyme=2)
  expect_equal(unlist(dC.biomassModel(parms=parms, y=y0, rateFlags=list(), poolAssignment=poolAssignment)),
               unlist(list(complex=as.numeric(parms['turnover_e']*y0['enzyme']),
                           enzyme=as.numeric(-parms['turnover_e']*y0['enzyme']))))
  
  parms <- unlist(list(turnover_b=0.2, basal=0.1))
  y0 <- unlist(list(complex=0, biomass=2))
  poolAssignment <- list(complex=1, biomass=2)
  expect_equal(unlist(dC.biomassModel(parms=parms, y=y0, rateFlags=list(), poolAssignment=poolAssignment)),
               unlist(list(complex=as.numeric(parms['turnover_b']*y0['biomass']),
                           biomass=as.numeric(-(parms['turnover_b']+parms['basal'])*y0['biomass']))))
})

test_that('dC.biomassModel returns correct enzyme production rates',{
  parms <- unlist(list(basal=0.1, 'enzProd'=0.01, 'enzCost'= 0.75))
  y0 <- unlist(list(biomass=2, enzyme=2))
  poolAssignment <- list(biomass=1, enzyme=2)
  expect_equal(unlist(dC.biomassModel(parms=parms, y=y0, rateFlags=list(prod='biomass'), poolAssignment=poolAssignment)),
               unlist(list(biomass=as.numeric(-((1+parms['enzCost'])*parms['enzProd']+parms['basal'])*y0['biomass']),
                           enzyme=as.numeric(parms['enzProd']*y0['biomass']))))
  
  expect_equal(unlist(dC.biomassModel(parms=parms, y=y0, rateFlags=list(prod='const'), poolAssignment=poolAssignment)),
               unlist(list(biomass=as.numeric(-(1+parms['enzCost'])*parms['enzProd']-parms['basal']*y0['biomass']),
                           enzyme=as.numeric(parms['enzProd']))))
  
  expect_equal(unlist(dC.biomassModel(parms=parms, y=y0, rateFlags=list(prod='uptake'), poolAssignment=poolAssignment)),
               unlist(list(biomass=as.numeric(-parms['basal']*y0['biomass']),
                           enzyme=as.numeric(0))))
  
})

test_that('R/dC.biomassModel.R returns correct protection rates', {
  parms <- unlist(list('sorb_rate'=0.1, 'desorb_rate'=0.5, 'carbonCapacity'=3))
  y0 <- unlist(list(protect_C=1, complex=4))
  poolAssignment <- list(protect_C=1, complex=2)
  expect_equal(as.numeric(unlist(dC.biomassModel(parms=parms, y=y0, rateFlags=list( sorb='carryCapacity'), poolAssignment=list(protect_C=1, complex=2)))),
               as.numeric(unlist(dC.biomassModel(parms=parms, y=y0, rateFlags=list( sorb='carryCapacity'), poolAssignment=list(protect_S=1, simple=2)))))
  
  expect_equal(unlist(dC.biomassModel(parms=parms, y=y0, rateFlags=list( sorb='carryCapacity'), poolAssignment=poolAssignment)),
               unlist(list(protect_C= as.numeric(y0['complex']*parms['sorb_rate']*(1-(y0['protect_C'])/parms['carbonCapacity']) - y0['protect_C']*parms['desorb_rate']), 
                           complex= -as.numeric(y0['complex']*parms['sorb_rate']*(1-(y0['protect_C'])/parms['carbonCapacity']) - y0['protect_C']*parms['desorb_rate'])
               )))
  
  y0 <- unlist(list(protect_C=1, complex=4, protect_S=1, simple=3))
  poolAssignment <- list(protect_C=1, complex=2, protect_S=3, simple=4)
  expect_equal(unlist(dC.biomassModel(parms=parms, y=y0, rateFlags=list( sorb='carryCapacity'), poolAssignment=poolAssignment)),
               unlist(list(protect_C= as.numeric(y0['complex']*parms['sorb_rate']*(1-(y0['protect_C']+y0['protect_S'])/parms['carbonCapacity']) - y0['protect_C']*parms['desorb_rate']), 
                           complex= -as.numeric(y0['complex']*parms['sorb_rate']*(1-(y0['protect_C']+y0['protect_S'])/parms['carbonCapacity']) - y0['protect_C']*parms['desorb_rate']),
                           protect_S=as.numeric(y0['simple']*parms['sorb_rate']*(1-(y0['protect_C']+y0['protect_S'])/parms['carbonCapacity']) - y0['protect_S']*parms['desorb_rate']),
                           simple= -as.numeric(y0['simple']*parms['sorb_rate']*(1-(y0['protect_C']+y0['protect_S'])/parms['carbonCapacity']) - y0['protect_S']*parms['desorb_rate'])
               )))
})

test_that('dC.biomassModel does dormancy correctly',{
  parms <- list('dormancy_rate'=0.1, 'S_dormancy'=0.1,'basal'=0.2, 'v_up' = 0.2, 'km_up' = 10, basal=0.01, cue=0.8)
  y0 <- list(biomass=1, dormant=4, simple=0)
  poolAssignment <- list(biomass=1, dormant=2, simple=3)
  
  expect_equal(unlist(dC.biomassModel(parms=unlist(parms), y=unlist(y0), rateFlags=list(uptake='MM'), poolAssignment=poolAssignment)),
               unlist(list(biomass=-y0$biomass*(parms$dormancy_rate+parms$basal),
                           dormant=y0$biomass*parms$dormancy_rate,
                           simple=0)))
  
  y0 <- list(biomass=0, dormant=4, substrate=1)
  expect_equal(unlist(dC.biomassModel(parms=unlist(parms), y=unlist(y0), rateFlags=list(uptake='MM'), poolAssignment=poolAssignment)),
               unlist(list(biomass=y0$dormant*parms$dormancy_rate,
                           dormant=-y0$dormant*parms$dormancy_rate,
                           simple=0)))
})

test_that('dC.biomassModel returns correct enzyme kinetics with biomass',{
  parms <- list('v_enz'=0.2, 'km_enz'=10, v_up=0.1, km_up=5, turnover_b=0.1, basal=0.01, cue=0.75)
  y0 <- list(simple=1, complex=2, biomass=3)
  poolAssignment <- list(simple=1, complex=2, biomass=3)
  
  expect_equal(unlist(dC.biomassModel(parms=unlist(parms), y=unlist(y0), rateFlags=list(enz='MM', uptake='Monod'), poolAssignment=poolAssignment)),
               unlist(list(simple=as.numeric(parms$v_enz*y0$complex*y0$biomass/(parms$km_enz+y0$complex) - parms$v_up*y0$simple*y0$biomass/(y0$simple+parms$km_up)), 
                           complex=as.numeric(parms$turnover_b*y0$biomass-
                                             parms$v_enz*y0$complex*y0$biomass/(parms$km_enz+y0$complex)), 
                           biomass=as.numeric(parms$cue*parms$v_up*y0$simple*y0$biomass/(y0$simple+parms$km_up)-(parms$turnover_b+parms$basal)*y0$biomass))))

  expect_equal(unlist(dC.biomassModel(parms=unlist(parms), y=unlist(y0), rateFlags=list(enz='revMM', uptake='Monod'), poolAssignment=poolAssignment)),
               unlist(list(simple=as.numeric(parms$v_enz*y0$complex*y0$biomass/(parms$km_enz+y0$biomass) - parms$v_up*y0$simple*y0$biomass/(y0$simple+parms$km_up)), 
                           complex=as.numeric(parms$turnover_b*y0$biomass-
                                                parms$v_enz*y0$complex*y0$biomass/(parms$km_enz+y0$biomass)), 
                           biomass=as.numeric(parms$cue*parms$v_up*y0$simple*y0$biomass/(y0$simple+parms$km_up)-(parms$turnover_b+parms$basal)*y0$biomass))))
})
