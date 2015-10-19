# Testing code for the RCMIP5 'measureFunctions.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code:
#   source("R/measureFunctions.R")
#   library(testthat)
#   test_file("tests/testthat/test_measureFunctions.R")

context("measureFunctions")

test_that("RMSE.measure handles bad inputs",{
  expect_error(RMSE.measure(data='not me', model=NA))
  expect_error(RMSE.measure(data=c(1), model=c(1,2)))
  expect_error(RMSE.measure(data=c(1,2), model=c(1)))
  expect_error(RMSE.measure())
})

test_that("RMSE.measure calculates correctly vectors",{
  expect_equal(RMSE.measure(data=c(1,2), model=c(1,2)), 0)
  expect_equal(RMSE.measure(data=c(1,2), model=c(1,1)), sqrt(sum((c(1,2)-c(1,1))^2)/2))
})

test_that("ll.measure handles bad inputs",{
  expect_error(ll.measure(data='not me', model=NA, sd=NA))
  expect_error(ll.measure(data=c(1), model=c(1,2), sd=c(1,1)))
  expect_error(ll.measure(data=c(1,2), model=c(1), sd=c(1,1)))
  expect_error(ll.measure(data=c(1,2), model=c(1,2), sd=c(1)))
  expect_error(ll.measure())
})

test_that("ll.measure calculates correctly vectors",{
  #2*sum(((data-model)/sd)^2)
  expect_equal(ll.measure(data=c(1,2), model=c(1,2), sd=c(1,1)), 0)
  expect_equal(ll.measure(data=c(1,2), model=c(1,1), sd=c(1,1)), 2*sum(((c(1,2)-c(1,1))/c(1,1))^2))
})