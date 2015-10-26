# Testing code for the RCMIP5 'measure.firstOrderModel.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code:
#   source("R/measure.firstOrderModel.R")
#   library(testthat)
#   test_file("tests/testthat/test_measure.firstOrderModel.R")

context("measure.firstOrderModel")

#source('R/runModel.R'); source('R/makeDecompMatrix.R'); source('R/publishedParameters.R'); source('R/dC.firstOrderModel.R'); source('R/measureFunctions.R')

test_that('measure.firstOrderModel dummy checks', {
  expect_error(measure.firstOrderModel(par=c(1,2)))
  expect_error(measure.firstOrderModel())
  
})

test_that('measure.firstOrderModel produces correct value',{
  par <- unlist(list('label1.a1'=0.1, tau1=180, tau2=100*365))
  
  C_bulk <- 1
  dt <- 1
  tauStr <- 'tau'
  transStr <- 'A'
  allocationStr <- 'a'
  
  
  
  relTime <- list(C1=4)
  temporalSplit <- c(10)
})
