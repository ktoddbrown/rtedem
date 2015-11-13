# Testing code for the RCMIP5 'measureModelFit.R' script

# Uses the testthat package
# See http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf
library(testthat)

# To run this code:
#   source("R/measureModelFit.R")
#   library(testthat)
#   test_file("tests/testthat/test_measureModelFit.R")

context("measureModelFit")

test_that('measureModelFit executes',{})

test_that('meausreModelFit evaluates correctly', {
  parls <- publishedParameters()[[1]]
  par <- c(a1=0.1, a2=0.1, a3=0.1, a4=0.1, parls$par)
  reactionNetwork <- parls$reactionNetwork
  reactionNetwork$to[is.na(reactionNetwork$to)] <- 'CO2'
  allocationFn <- function(par, C_bulk=1){with(as.list(par), C_bulk*c(C1=a1, C2=a2, C3=a3, C4=a4, C5=1-sum(c(a1, a2, a3, a4)), CO2=0))}
  
  
  refData <- data.frame(time=1:10, variable=rep('CO2', length=10), mean=1-exp(-1/180*1:10), sd=(1-exp(-1/180*1:10))*0.1)
  C1 <- exp(-1/180*1:10)
  C1 <- C1/C1[2]
  refData <- rbind(refData, data.frame(time=c(2, 7, 10), variable='relC1', 
                                       mean=C1[c(2, 7, 10)], sd=0.1))
  
  expect_equal(measureModelFit(par=par, refData=refData, reactionNetwork, allocationFn), 857.9174, tolerance=1e-2)
})