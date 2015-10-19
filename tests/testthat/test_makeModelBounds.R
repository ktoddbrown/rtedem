# Testing code for the RCMIP5 'makeModelBounds.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code:
#   source("R/makeModelBounds.R")
#   library(testthat)
#   test_file("tests/testthat/test_makeModelBounds.R")

context("makeModelBounds")

test_that("makeModelBounds deals with missing data.",{
  expect_error(makeModelBounds(modelBase=NULL))
  expect_error(makeModelBounds(modelBase=c(1,2)))
})

test_that("makeModelBounds check with simple case",{
  ref <- list(test1=data.frame(name=c('K1', 'K2', 'A2'), true=c(-10, -100, 0.5), max=c(0, -55, 1), min=c(-55, -190, 0)))
  temp <- list(test1=list(tau=c(10, 100), trans=data.frame(name='A2', val=0.5)))
  
  expect_equal(makeModelBounds(modelBase=temp), ref)
})