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
  expect_error(measure.first)
})
