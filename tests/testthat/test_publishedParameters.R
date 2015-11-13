# Testing code for the RCMIP5 'publishedParameters.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code:
#   source("R/publishedParameters.R")
#   library(testthat)
#   test_file("tests/testthat/test_publishedParameters.R")

context("publishedParameters")

test_that("publishedParameters handles bad inputs",{
  expect_error(publishedParameters(nameArr=c(1,2,3)))
})
