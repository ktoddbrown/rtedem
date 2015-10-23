# Testing code for the RCMIP5 'dC.SCBModel.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code:
#   source("R/dC.SCBModel.R")
#   library(testthat)
#   test_file("tests/testthat/test_dC.SCBModel.R")

context("dC.SCBModel")

test_that('dCSCBModel produces expected errors',{
  expect_error(dC.SCBModel())
  expect_error(dC.SCBModel(parms=list(not=1, real=2), t=1, y=c(1,1,1,1)))
  expect_error(dC.SCBModel(parms=list(not=1, real=2), t=1, y=1))
  par <- list('v_enz'=0.2, 'km_enz'=10,
              'v_up'=1, 'km_up'=2,
              'cue'=0.5, 'basal' = 0.01,
              'turnover_b'=0.5, 'turnover_e'=0.1)
  expect_error(dC.SCBModel(parms=par, t=1, y=1))
})

test_that('dC.SCBModel produces correct outputs', {
  par <- list('v_enz'=0.2, 'km_enz'=10,
                  'v_up'=1, 'km_up'=2,
                  'cue'=0.5, 'basal' = 0.01,
                  'turnover_b'=0.5, 'turnover_e'=0.1)
  ####compare prod=uptake
  test <- dC.SCBModel(parms=par, t=0, y=c(1,2,3), rateFlags=list(enz='MM', uptake='MM'))
  expect_true(all(abs(unlist(test) - c(-0.90,  1.40, -1.03)) < 1e-3))
  
  test <- dC.SCBModel(parms=par, t=0, y=c(1,2,3), rateFlags=list(enz='revMM', uptake='MM'))
  expect_true(all(unlist(test) - c(-0.9076923,  1.4076923, -1.0300000) < 1e-3))
   
  test <- dC.SCBModel(parms=par, t=0, y=c(1,2,3), rateFlags=list(enz='multi', uptake='MM'))
   expect_true(all(unlist(test) - c(0.20,  0.30, -1.03) < 1e-3))
   
#   #####compare prod=biomass
   test <- dC.SCBModel(parms=par, t=0, y=c(1,2,3), rateFlags=list(enz='MM', uptake='revMM'))
   expect_true(all(abs(unlist(test) - c(-0.50,  1.40, -1.23)) < 1e-3))
   
   test <- dC.SCBModel(parms=par, t=0, y=c(1,2,3), rateFlags=list(enz='revMM', uptake='revMM'))
   expect_true(all(abs(unlist(test) - c(-0.5076923,  1.4076923, -1.2300000)) < 1e-3))
   
   test <- dC.SCBModel(parms=par, t=0, y=c(1,2,3), rateFlags=list(enz='multi', uptake='revMM'))
   expect_true(all(abs(unlist(test) - c(0.60, 0.30, -1.23)) < 1e-3))
  
})