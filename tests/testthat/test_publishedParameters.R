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

test_that("pulblishedParameters returns tau, trans, type, and citation",{
  parList <- publishedParameters()
  for(modelStr in names(parList)){
    expect_true('tau' %in% names(parList[[modelStr]]), info=modelStr)
    expect_true('trans' %in% names(parList[[modelStr]]), info=modelStr)
    expect_true('type' %in% names(parList[[modelStr]]), info=modelStr)
    expect_true('citation' %in% names(parList[[modelStr]]), info=modelStr)
  }
})

test_that("publishedParameters returns reasonable trans parameters",{
  parList <- publishedParameters()
  for(modelStr in names(parList)){
    numPools <- length(parList[[modelStr]]$tau)
    expect_less_than(length(parList[[modelStr]]$trans), numPools^2)
    transCheck <- parList[[modelStr]]$trans
    transCheck$index <- as.numeric(gsub('A', '', transCheck$name))
    transMatrix <- matrix(0, nrow=numPools, ncol=numPools)
    transMatrix[transCheck$index] <- transCheck$val
    expect_identical(diag(transMatrix), rep(0, numPools), info=modelStr)
    expect_true(all(colSums(transMatrix) < rep(1, numPools)), info=modelStr)
    expect_true(all(transCheck$val <= 1), info=modelStr)
  }
})