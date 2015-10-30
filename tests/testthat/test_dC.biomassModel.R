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
  
})

test_that('dC.biomassModel produces correct outputs with a simple-complex-biomass model', {
  par <- unlist(list('v_enz'=0.2, 'km_enz'=10,
                  'v_up'=1, 'km_up'=2,
                  'cue'=0.5, 'basal' = 0.01,
                  'turnover_b'=0.5, 'turnover_e'=0.1))
  poolAssignment <- list(simple=1, complex=2, biomass=3)
  ####compare prod=uptake
  test <- dC.biomassModel(parms=par, t=0, y=c(1,2,3), rateFlags=list(enz='MM', uptake='MM'), poolAssignment=poolAssignment)
  expect_true(all(abs(unlist(test) - c(-0.90,  1.40, -1.03)) < 1e-3))
  
  test <- dC.biomassModel(parms=par, t=0, y=c(1,2,3), rateFlags=list(enz='revMM', uptake='MM'), poolAssignment=poolAssignment)
  expect_true(all(unlist(test) - c(-0.9076923,  1.4076923, -1.0300000) < 1e-3))
   
  test <- dC.biomassModel(parms=par, t=0, y=c(1,2,3), rateFlags=list(enz='multi', uptake='MM'), poolAssignment=poolAssignment)
   expect_true(all(unlist(test) - c(0.20,  0.30, -1.03) < 1e-3))
   
#   #####compare prod=biomass
   test <- dC.biomassModel(parms=par, t=0, y=c(1,2,3), rateFlags=list(enz='MM', uptake='revMM'), poolAssignment=poolAssignment)
   expect_true(all(abs(unlist(test) - c(-0.50,  1.40, -1.23)) < 1e-3))
   
   test <- dC.biomassModel(parms=par, t=0, y=c(1,2,3), rateFlags=list(enz='revMM', uptake='revMM'), poolAssignment=poolAssignment)
   expect_true(all(abs(unlist(test) - c(-0.5076923,  1.4076923, -1.2300000)) < 1e-3))
   
   test <- dC.biomassModel(parms=par, t=0, y=c(1,2,3), rateFlags=list(enz='multi', uptake='revMM'), poolAssignment=poolAssignment)
   expect_true(all(abs(unlist(test) - c(0.60, 0.30, -1.23)) < 1e-3))
  
})

test_that('dC.biomassModel produces correct outputs with a simple-complex-enzyme-biomass model', {
  par <- unlist(list('v_enz'=0.2, 'km_enz'=10,
                     'v_up'=1, 'km_up'=2,
                     'enzProd'=0.01,
                     'enzCost'=0.75, 'cue'=0.5, 'basal' = 0.01,
                     'turnover_b'=0.5, 'turnover_e'=0.1))
  poolAssignment <- list(simple=1, complex=2, enzyme=3, biomass=4)
  ####compare prod=uptake
  test <- dC.biomassModel(parms=par, t=0, y=c(1,1,1,1), rateFlags=list(enz='MM', uptake='Monod',  prod='uptake'), poolAssignment=poolAssignment)
  expect_true(all(abs(unlist(test) - c(-0.31515152,  0.58181818, -0.09666667, -0.34916667 
  )) < 1e-3))
  
  test <- dC.biomassModel(parms=par, t=0, y=c(1,2,3,4), rateFlags=list(enz='revMM', uptake='Monod',  prod='uptake'), poolAssignment=poolAssignment)
  expect_true(all(unlist(test) - c(-1.2410256,  2.2076923, -0.2866667, -1.3966667 ) < 1e-3))
  
  test <- dC.biomassModel(parms=par, t=0, y=c(1,2,3,4), rateFlags=list(enz='multi', uptake='Monod',  prod='uptake'), poolAssignment=poolAssignment)
  expect_true(all(unlist(test) - c(-0.1333333,  1.1000000, -0.2866667, -1.3966667 ) < 1e-3))
  
  #   #####compare prod=biomass
  test <- dC.biomassModel(parms=par, t=0, y=c(1,1,1,1), rateFlags=list(enz='MM', uptake='Monod',  prod='biomass'), poolAssignment=poolAssignment)
  expect_true(all(abs(unlist(test) - c(-0.3151515,  0.5818182, -0.0900000, -0.3608333 )) < 1e-3))
  
  test <- dC.biomassModel(parms=par, t=0, y=c(1,2,3,4), rateFlags=list(enz='revMM', uptake='Monod',  prod='biomass'), poolAssignment=poolAssignment)
  expect_true(all(abs(unlist(test) - c(-1.241026,  2.207692, -0.260000, -1.443333 )) < 1e-3))
  
  test <- dC.biomassModel(parms=par, t=0, y=c(1,2,3,4), rateFlags=list(enz='multi', uptake='Monod',  prod='biomass'), poolAssignment=poolAssignment)
  expect_true(all(abs(unlist(test) - c(-0.1333333,  1.1000000, -0.2600000, -1.4433333 )) < 1e-3))
})


test_that('dC.biomassModel produces correct outputs with a simple-complex-enzyme-biomass-dorment model ', {
  par <- unlist(list('v_enz'=0.2, 'km_enz'=10,
                     'v_up'=1, 'km_up'=2,
                     'enzProd'=0.01,
                     'enzCost'=0.75, 'cue'=0.5, 'basal' = 0.01,
                     'turnover_b'=0.5, 'turnover_e'=0.1, 'dormancy_rate'=0.3))
  poolAssignment <- list(simple=1, complex=2, enzyme=3, biomass=4, dormant=5)
  ####compare prod=uptake
  test <- dC.biomassModel(parms=par, t=0, y=c(1,1,1,1,1), rateFlags=list(enz='MM', uptake='Monod',  prod='uptake'), poolAssignment=poolAssignment)
  expect_true(all(abs(unlist(test) - c(-0.31515152,  0.58181818, -0.09666667, -0.64916667,  0.30000000)) < 1e-3))
  
  test <- dC.biomassModel(parms=par, t=0, y=c(1,2,3,4, 1), rateFlags=list(enz='revMM', uptake='Monod',  prod='uptake'), poolAssignment=poolAssignment)
  expect_true(all(unlist(test) - c(-1.2410256,  2.2076923, -0.2866667, -2.5966667,  1.2000000  ) < 1e-3))
  
  test <- dC.biomassModel(parms=par, t=0, y=c(1,2,3,4, 5), rateFlags=list(enz='multi', uptake='Monod',  prod='uptake'), poolAssignment=poolAssignment)
  expect_true(all(unlist(test) - c(-0.1333333,  1.1000000, -0.2866667, -2.5966667,  1.2000000  ) < 1e-3))
  
  #   #####compare prod=biomass
  test <- dC.biomassModel(parms=par, t=0, y=c(1,1,1,1,1), rateFlags=list(enz='MM', uptake='Monod',  prod='biomass'), poolAssignment=poolAssignment)
  expect_true(all(abs(unlist(test) - c(-0.3151515,  0.5818182, -0.0900000, -0.6608333,  0.3000000 )) < 1e-3))
  
  test <- dC.biomassModel(parms=par, t=0, y=c(1,2,3,4, 1), rateFlags=list(enz='revMM', uptake='Monod',  prod='biomass'), poolAssignment=poolAssignment)
  expect_true(all(abs(unlist(test) - c(-1.241026,  2.207692, -0.260000, -2.643333,  1.200000 )) < 1e-3))
  
  test <- dC.biomassModel(parms=par, t=0, y=c(1,2,3,4, 1), rateFlags=list(enz='multi', uptake='Monod',  prod='biomass'), poolAssignment=poolAssignment)
  expect_true(all(abs(unlist(test) - c(-0.1333333,  1.1000000, -0.2600000, -2.6433333,  1.2000000 )) < 1e-3))
  
})

test_that('dC.biomassModel produces correct outputs with a simple-complex-enzyme-biomass-dorment-occluded model ', {
  par <- unlist(list('v_enz'=0.2, 'km_enz'=10,
                     'v_up'=1, 'km_up'=2,
                     'enzProd'=0.01,
                     'enzCost'=0.75, 'cue'=0.5, 'basal' = 0.01,
                     'turnover_b'=0.5, 'turnover_e'=0.1, 'dormancy_rate'=0.3,
                     'sorb_rate'=0.2, 'desorb_rate'=0.1, 'carbonCapacity'=3))
  poolAssignment <- list(simple=1, complex=2, enzyme=3, biomass=4, dormant=5, protect_S=6, protect_C=7)
  ####compare prod=uptake
  test <- dC.biomassModel(parms=par, t=0, y=c(1,1,1,1,1,1,1), rateFlags=list(enz='MM', uptake='Monod',  prod='uptake', sorb='carryCapacity'), poolAssignment=poolAssignment)
  expect_true(all(abs(unlist(test) - c(-0.28181818,  0.61515152, -0.09666667, -0.64916667,  0.30000000, -0.03333333, -0.03333333)) < 1e-3))
  
  test <- dC.biomassModel(parms=par, t=0, y=c(1,2,3,4,1,1,1), rateFlags=list(enz='revMM', uptake='Monod',  prod='uptake', sorb='carryCapacity'), poolAssignment=poolAssignment)
  expect_true(all(unlist(test) - c(-1.20769231,  2.17435897, -0.28666667, -2.59666667,  1.20000000, -0.03333333,  0.03333333 ) < 1e-3))
  
  test <- dC.biomassModel(parms=par, t=0, y=c(1,2,3,4,5, 1, 1), rateFlags=list(enz='multi', uptake='Monod',  prod='uptake', sorb='carryCapacity'), poolAssignment=poolAssignment)
  expect_true(all(unlist(test) - c(-0.10000000,  1.06666667, -0.28666667, -2.59666667,  1.20000000, -0.03333333,  0.03333333  ) < 1e-3))
  
  #   #####compare prod=biomass
  test <- dC.biomassModel(parms=par, t=0, y=c(1,1,1,1,1, 1, 1), rateFlags=list(enz='MM', uptake='Monod',  prod='biomass', sorb='carryCapacity'), poolAssignment=poolAssignment)
  expect_true(all(abs(unlist(test) - c(-0.28181818,  0.61515152, -0.09000000, -0.66083333,  0.30000000, -0.03333333, -0.03333333)) < 1e-3))
  
  test <- dC.biomassModel(parms=par, t=0, y=c(1,2,3,4, 1, 1, 1), rateFlags=list(enz='revMM', uptake='Monod',  prod='biomass', sorb='carryCapacity'), poolAssignment=poolAssignment)
  expect_true(all(abs(unlist(test) - c(-1.20769231,  2.17435897, -0.26000000, -2.64333333,  1.20000000, -0.03333333,  0.03333333)) < 1e-3))
  
  test <- dC.biomassModel(parms=par, t=0, y=c(1,2,3,4, 1, 1, 1), rateFlags=list(enz='multi', uptake='Monod',  prod='biomass', sorb='carryCapacity'), poolAssignment=poolAssignment)
  expect_true(all(abs(unlist(test) - c(-0.10000000,  1.06666667, -0.26000000, -2.64333333,  1.20000000, -0.03333333,  0.03333333)) < 1e-3))
  
})
