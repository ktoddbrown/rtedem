# Testing code for the RCMIP5 'dC.SCEADModel.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code:
#   source("R/dC.SCEADModel.R")
#   library(testthat)
#   test_file("tests/testthat/test_dC.SCEADModel.R")

context("dC.SCEADModel")

test_that('dCSCEADModel produces expected errors',{
  expect_error(dC.SCEADModel())
  expect_error(dC.SCEADModel(parms=list(not=1, real=2), t=1, y=c(1,1,1,1)))
  expect_error(dC.SCEADModel(parms=list(not=1, real=2), t=1, y=1))
  par <- unlist(list('v_enz'=0.2, 'km_enz'=10,
              'v_up'=1, 'km_up'=2,
              'enzProd'=0.01,
              'enzCost'=0.75, 'cue'=0.5, 'basal' = 0.01,
              'turnover_b'=0.5, 'turnover_e'=0.1, 'dormant_rate'=0.3))
  expect_error(dC.SCEADModel(parms=par, t=1, y=1))
})

test_that('dC.SCEADModel produces correct outputs', {
  par <- unlist(list('v_enz'=0.2, 'km_enz'=10,
                  'v_up'=1, 'km_up'=2,
                  'enzProd'=0.01,
                  'enzCost'=0.75, 'cue'=0.5, 'basal' = 0.01,
                  'turnover_b'=0.5, 'turnover_e'=0.1, 'dormancy_rate'=0.3))
  ####compare prod=uptake
  test <- dC.SCEADModel(parms=par, t=0, y=c(1,1,1,1,1), rateFlags=list(enz='MM', uptake='Monod',  prod='uptake'))
  expect_true(all(abs(unlist(test) - c(-0.31515152,  0.58181818, -0.09666667, -0.64916667,  0.30000000)) < 1e-3))
  
test <- dC.SCEADModel(parms=par, t=0, y=c(1,2,3,4, 1), rateFlags=list(enz='revMM', uptake='Monod',  prod='uptake'))
  expect_true(all(unlist(test) - c(-1.2410256,  2.2076923, -0.2866667, -2.5966667,  1.2000000  ) < 1e-3))
   
test <- dC.SCEADModel(parms=par, t=0, y=c(1,2,3,4, 5), rateFlags=list(enz='multi', uptake='Monod',  prod='uptake'))
   expect_true(all(unlist(test) - c(-0.1333333,  1.1000000, -0.2866667, -2.5966667,  1.2000000  ) < 1e-3))
   
#   #####compare prod=biomass
   test <- dC.SCEADModel(parms=par, t=0, y=c(1,1,1,1,1), rateFlags=list(enz='MM', uptake='Monod',  prod='biomass'))
   expect_true(all(abs(unlist(test) - c(-0.3151515,  0.5818182, -0.0900000, -0.6608333,  0.3000000 )) < 1e-3))
   
   test <- dC.SCEADModel(parms=par, t=0, y=c(1,2,3,4, 1), rateFlags=list(enz='revMM', uptake='Monod',  prod='biomass'))
   expect_true(all(abs(unlist(test) - c(-1.241026,  2.207692, -0.260000, --2.643333,  1.200000 )) < 1e-3))
   
   test <- dC.SCEADModel(parms=par, t=0, y=c(1,2,3,4, 1), rateFlags=list(enz='multi', uptake='Monod',  prod='biomass'))
   expect_true(all(abs(unlist(test) - c(-0.1333333,  1.1000000, -0.2600000, -2.6433333,  1.2000000 )) < 1e-3))
  
})