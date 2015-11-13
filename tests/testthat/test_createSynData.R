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
  parls <- publishedParameters()[[1]]
  par <- c(a1=0.1, a2=0.1, a3=0.1, a4=0.1, parls$par)
  reactionNetwork <- parls$reactionNetwork
  reactionNetwork$to[is.na(reactionNetwork$to)] <- 'CO2'
  allocationFn <- function(par, C_bulk=1){with(as.list(par), C_bulk*c(C1=a1, C2=a2, C3=a3, C4=a4, C5=1-sum(c(a1, a2, a3, a4)), CO2=0))}
  
  expect_silent(temp <- createSynData(par, reactionNetwork, allocationFn=allocationFn, timeArr=0:10, dt=1, relTime=c(C1=2), relSd=0.1, verbose=FALSE))
})