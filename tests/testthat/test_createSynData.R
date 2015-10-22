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
  synData <- createSynData(par=unlist(list(tau1=180)), timeArr=2^(seq(0, 10, length=50)))
})