# Testing code for the RCMIP5 'createSynData.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code:
#   source("R/createSynData.R")
#   library(testthat)
#   test_file("tests/testthat/test_createSynData.R")

context("createSynData")

test_that('createSynData does it run on a one pool model', {
  expect_silent(synData <- createSynData(par=unlist(list(tau1=180)), timeArr=2^(seq(0, 10, length=50)), tauStr='tau'))
  
   par <- unlist(list('v_enz'=0.2, 'km_enz'=10,
                      'v_up'=1, 'km_up'=2,
                      'cue'=0.5, 'basal' = 0.01,
                      'turnover_b'=0.5, 'turnover_e'=0.1,
                      a1=0.1, a2=0.8))
   expect_silent(synData <- createSynData(par, timeArr=2^(seq(0, 10, length=50)), cModel=dC.SCBModel))
   expect_silent(synData <- createSynData(par, timeArr=2^(seq(0, 10, length=50)), cModel=dC.SCBModel, relTime=list('biomass'=4)))
})