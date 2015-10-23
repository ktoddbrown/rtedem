# Testing code for the RCMIP5 'dC.SCEBModel.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code:
#   source("R/dC.SCEBModel.R")
#   library(testthat)
#   test_file("tests/testthat/test_dC.SCEBModel.R")

context("dC.SCEBModel")

test_that('dCSCEBModel produces expected errors',{
  expect_error(dC.SCEBModel())
  expect_error(dC.SCEBModel(parms=list(not=1, real=2), t=1, y=c(1,1,1,1)))
  expect_error(dC.SCEBModel(parms=list(not=1, real=2), t=1, y=1))
  par <- list('v_enz'=0.2, 'km_enz'=10,
              'v_up'=1, 'km_up'=2,
              'enzProd'=0.01,
              'enzCost'=0.75, 'cue'=0.5, 'basal' = 0.01,
              'turnover_b'=0.5, 'turnover_e'=0.1)
  expect_error(dC.SCEBModel(parms=par, t=1, y=1))
})

test_that('dC.SCEBModel produces correct outputs', {
  par <- list('v_enz'=0.2, 'km_enz'=10,
                  'v_up'=1, 'km_up'=2,
                  'enzProd'=0.01,
                  'enzCost'=0.75, 'cue'=0.5, 'basal' = 0.01,
                  'turnover_b'=0.5, 'turnover_e'=0.1)
  ####compare prod=uptake
  test <- dC.SCEBModel(parms=par, t=0, y=c(1,1,1,1), rateFlags=list(enz='MM', uptake='Monod',  prod='uptake'))
  expect_true(all(abs(unlist(test) - c(-0.315, 0.581, -0.349, -0.096)) < 1e-3))
  
test <- dC.SCEBModel(parms=par, t=0, y=c(1,2,3,4), 
                                    rateFlags=list(enz='revMM', uptake='Monod',  prod='uptake'))
  expect_true(all(unlist(test) - c(-1.241, 2.207, -1.396, -0.286) < 1e-3))
   
test <- dC.SCEBModel(parms=par, t=0, y=c(1,2,3,4), 
                     rateFlags=list(enz='multi', uptake='Monod',  prod='uptake'))
   expect_true(all(unlist(test) - c(-0.1333333,  1.1000000, -1.3966667, -0.2866667) < 1e-3))
   
#   #####compare prod=biomass
   test <- dC.SCEBModel(parms=par, t=0, y=c(1,1,1,1), 
                        rateFlags=list(enz='MM', uptake='Monod',  prod='biomass'))
   expect_true(all(abs(unlist(test) - c(-0.3151515,  0.5818182, -0.3608333, -0.09000)) < 1e-3))
   
   test <- dC.SCEBModel(parms=par, t=0, y=c(1,2,3,4), 
                         rateFlags=list(enz='revMM', uptake='Monod',  prod='biomass'))
   expect_true(all(abs(unlist(test) - c(-1.241026,  2.207692, -1.443333, -0.260000)) < 1e-3))
   
   test <- dC.SCEBModel(parms=par, t=0, y=c(1,2,3,4), 
                        rateFlags=list(enz='multi', uptake='Monod',  prod='biomass'))
   expect_true(all(abs(unlist(test) - c(-0.1333333,  1.1000000, -1.4433333, -0.2600000)) < 1e-3))
  
})