# Testing code for the RCMIP5 'makeDecompMatrix.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code:
#   source("R/makeDecompMatrix.R")
#   library(testthat)
#   test_file("tests/testthat/test_makeDecompMatrix.R")

context("makeDecompMatrix")

test_that("makeDecomMatrix deals with bad inputs",{
  expect_error(makeDecompMatrix())
  expect_error(makeDecompMatrix(par=c(1,2)))
  expect_error(makeDecompMatrix(par=unlist(list(a=1, b=2))))
})

test_that("makeDecompMatrix correctly calculates test matrix", {
  ans <- c(-0.1, 0.05, 0, -0.01)
  dim(ans) <- c(2,2)
  expect_equal(makeDecompMatrix(unlist(list(tau1=10, tau2=100, A2=0.5))), ans)
})